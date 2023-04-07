use std::fmt::{self, Debug, Formatter};

use ecow::{eco_format, EcoVec, eco_vec};

use super::{Array, Cast, Dict, Str, Value};
use crate::diag::{bail, At, SourceResult};
use crate::syntax::{Span, Spanned};
use crate::util::pretty_array_like;

/// Evaluated arguments to a function.
#[derive(Clone, PartialEq, Hash)]
pub struct Args {
    /// The span of the whole argument list.
    pub span: Span,
    /// The positional and named arguments.
    pub items: EcoVec<Arg>,
}

/// An argument to a function call: `12` or `draw: false`.
#[derive(Clone, PartialEq, Hash)]
pub struct Arg {
    /// The span of the whole argument.
    pub span: Span,
    /// The value of the argument.
    pub value: ArgValue,
}

#[derive(Clone, PartialEq, Hash)]
pub enum ArgValue {
    /// A positional argument.
    Pos(Spanned<Value>),
    /// A named argument.
    Named(Str, Spanned<Value>),
}

impl Args {
    /// Create positional arguments from a span and values.
    pub fn new(span: Span, values: impl IntoIterator<Item = Value>) -> Self {
        let items = values
            .into_iter()
            .map(|value| Arg {
                span,
                value: ArgValue::Pos(Spanned::new(value, span)),
            })
            .collect();
        Self { span, items }
    }

    /// Push a positional argument.
    pub fn push(&mut self, span: Span, value: Value) {
        self.items.push(Arg {
            span: self.span,
            value: ArgValue::Pos(Spanned::new(value, span)),
        })
    }

    /// Extract the positional arguments as an array.
    pub fn to_pos(&self) -> Array {
        self.items
            .iter()
            .filter_map(|item| match &item.value {
                ArgValue::Pos(value) => Some(value.v.clone()),
                _ => None,
            })
            .collect()
    }

    /// Extract the named arguments as a dictionary.
    pub fn to_named(&self) -> Dict {
        self.items
            .iter()
            .filter_map(|item| match &item.value {
                ArgValue::Named(name, value) => Some((name.clone(), value.v.clone())),
                _ => None,
            })
            .collect()
    }

    /// Extract the positional arguments as an array.
    pub fn argc(&self) -> usize {
        self.items
            .iter()
            .filter(|item| matches!(item.value, ArgValue::Pos(_)))
            .count()
    }
}

/// Evaluated arguments to a function.
#[derive(Clone, PartialEq, Hash)]
pub struct ArgsAccessor {
    //// The arguments to access.
    pub args: Args,
    /// The number of expected positional arguments.
    pub num_pos_params: usize,
    /// The arguments that were taken out.
    pub sink: Args,
}

impl ArgsAccessor {
    /// Create accessor for Arguments
    pub fn new(args: &Args, num_pos_params: usize) -> Self {
        Self { args: args.clone(), num_pos_params, sink: Args { span: args.span, items: eco_vec![] } }
    }

    /// Consume and cast the first positional argument if there is one.
    pub fn eat<T>(&mut self) -> SourceResult<Option<T>>
    where
        T: Cast<Spanned<Value>>,
    {
        let pos = self.args.items.iter().position(|arg| matches!(arg.value, ArgValue::Pos(_)));
        if let Some(pos) = pos {
            if let ArgValue::Pos(value) = self.args.items.remove(pos).value {
                return T::cast(value.clone()).at(value.span).map(Some);
            }
        }
        Ok(None)
    }

    /// Consume and cast the first positional argument.
    ///
    /// Returns a `missing argument: {what}` error if no positional argument is
    /// left.
    pub fn expect<T>(&mut self, what: &str) -> SourceResult<T>
    where
        T: Cast<Spanned<Value>>,
    {
        match self.eat()? {
            Some(v) => Ok(v),
            None => bail!(self.args.span, "missing argument: {}", what),
        }
    }

    /// Consume and cast the all positional arguments.
    ///
    /// Returns a `missing argument: {what}` error if no positional argument is
    /// left.
    pub fn expectall<T>(&mut self, what: &str) -> SourceResult<Vec<T>>
    where
        T: Cast<Spanned<Value>>,
    {
        let mut list = vec![];
        while !self.args.items.is_empty(){
            list.push(self.expect(what)?);
        }
        Ok(list)
    }

    /// Find and consume the first castable positional argument.
    pub fn find<T>(&mut self) -> SourceResult<Option<T>>
    where
        T: Cast<Spanned<Value>>,
    {
        let pos = self.args.items.iter().position(|arg| matches!(arg.value, ArgValue::Named(_, _)));
        if let Some(pos) = pos {
            if let ArgValue::Named(_, value) = self.args.items.remove(pos).value {
                return T::cast(value.clone()).at(value.span).map(Some);
            }
        }
        Ok(None)
    }

    /// Cast and remove the value for the given named argument, returning an
    /// error if the conversion fails.
    pub fn named<T>(&mut self, name: &str) -> SourceResult<Option<T>>
    where
        T: Cast<Spanned<Value>>,
    {
        // We don't quit once we have a match because when multiple matches
        // exist, we want to remove all of them and use the last one.
        let mut i = 0;
        let mut found = None;
        while i < self.args.items.len() {
            if let ArgValue::Named(s, value) = &self.args.items[i].value {
                if s.as_str() != name {
                    continue;
                }
                found = Some(T::cast(value.clone()).at(value.span)?);
            } else {
                i += 1;
            }
        }
        Ok(found)
    }

    /// Same as named, but with fallback to find.
    pub fn named_or_find<T>(&mut self, name: &str) -> SourceResult<Option<T>>
    where
        T: Cast<Spanned<Value>>,
    {
        match self.named(name)? {
            Some(value) => Ok(Some(value)),
            None => self.find(),
        }
    }

    pub fn take_sink(&mut self) {
        if let Some(sink_size) = self.args.argc().checked_sub(self.num_pos_params) {
            let vec = self.args.items.to_vec();
            let (left, right) = vec.split_at(sink_size);
            self.args.items = right.into();

            self.sink.items.extend_from_slice(left);
        }
    }

    pub fn take_named(&mut self) {
        let mut list = vec![];
        for arg in &self.args.items {
            if let ArgValue::Named(_, _) = arg.value {
                list.push(arg.clone());
            }
        }

        self.sink.items.extend(list);
    }

    /// Return an "unexpected argument" error if there is any remaining
    /// argument.
    pub fn finish(self) -> SourceResult<()> {
        if let Some(arg) = self.args.items.first() {
            bail!(arg.span, "unexpected argument");
        }
        Ok(())
    }
}

impl Debug for Args {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let pieces: Vec<_> =
            self.items.iter().map(|arg| eco_format!("{arg:?}")).collect();
        f.write_str(&pretty_array_like(&pieces, false))
    }
}

impl Debug for Arg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.value {
            ArgValue::Named(name, value) => {
                f.write_str(&name)?;
                f.write_str(": ")?;
                Debug::fmt(&value.v, f)
            }
            ArgValue::Pos(value) => Debug::fmt(&value.v, f),
        }
    }
}
