use std::{convert::Infallible, result::Result, sync::OnceLock};

#[macro_export]
macro_rules! bake {
    ($mod_name:ident; $x:ident @glob $name:ident $(::$rest:ident)+ $(as $alias:ident)?) => {$crate::bake!($mod_name; $x @glob $($rest)::+ $(as $alias)?)};

    // include foo
    // include foo as alias
    ($mod_name:ident; $x:ident @glob include $name:ident $(::$rest:ident)* $(as $alias:ident)?) => {};
    // pub include foo
    // pub include foo as alias
    ($mod_name:ident; $x:ident @glob pub include $name:ident $(::$rest:ident)* $(as $alias:ident)?) => {$crate::bake!($mod_name; $x @glob $name $(::$rest)* $(as $alias)?)};

    // use some::mod::foo as alias
    // use some::mod::foo
    ($mod_name:ident; $x:ident @glob use $($rest:ident)::* $(as $alias:ident)?) => {};
    // pub use some::mod::foo as alias
    ($mod_name:ident; $x:ident @glob pub use $($rest:ident)::* as $alias:ident) => {let $x = $x.chain([$alias]);};

    // pub use some::mod::foo
    ($mod_name:ident; $x:ident @glob pub use $path:ident :: $($rest:ident)::+ $(as $alias:ident)?) =>
        {$crate::bake!($mod_name; $x; $path; @glob_p pub use $($rest)::+ $(as $alias)?)};
    ($mod_name:ident; $x:ident; $($p:ident)*; @glob_p pub use $path:ident :: $($rest:ident)::+  $(as $alias:ident)?) =>
        {$crate::bake!($mod_name; $x; $($p ::)* $path; @glob_p pub use $($rest)::+ $(as $alias)?)};
    ($mod_name:ident; $x:ident; $($path:ident)::*; @glob_p pub use $name:ident  $(as $alias:ident)?) =>
        {let $x = $x.chain([super::$($path ::)* $mod_name :: $name]);};

    // use some::mod::*
    ($mod_name:ident; $x:ident @glob use $($rest:ident)::* * $(as $alias:ident)?) => {/* Don't export non-public glob imports */};
    // pub use some::mod::*
    ($mod_name:ident; $x:ident @glob pub use $($rest:ident)::* ::* $(as $alias:ident)?) => {let $x = $x.chain($($rest)::*::$mod_name::ALL());};
    // pub mod foo
    ($mod_name:ident; $x:ident @glob $name:ident) => {let $x = $x.chain([$name]);};
    // pub mod foo as alias
    ($mod_name:ident; $x:ident @glob $name:ident as $alias:ident) => {let $x = $x.chain([$alias]);};

    ($($p:ident)? @resn $name:ident; $path:path) => {#[allow(non_upper_case_globals)] $($p)? const $name: Include = $path;};
    ($($p:ident)? @resn $pre:ident :: $rest:path; $path:path) => {$crate::bake!(@resn $rest; $path);};
    ($($p:ident)? @resn $rest:path; $path:path) => {};
    ($($p:ident)? @resn $name:path as $alias:ident; $path:path) => {#[allow(non_upper_case_globals)] $($p)? const $alias: Include = $path;};
    ($pre:literal, $t:path, $suf:literal; @i pub use $path:path $(as $alias:ident)?) => {
        $crate::bake!(pub @resn $path $(as $alias)?; $path);
    };
    ($pre:literal, $t:path, $suf:literal; @i pub use $($path:tt)::* $(as $alias:ident)?) => {
        pub use $($path)::*;
    };
    ($pre:literal, $t:path, $suf:literal; @i use $path:path $(as $alias:ident)?) => {
        $crate::bake!(@resn $path $(as $alias)?; $path);
    };
    ($pre:literal, $t:path, $suf:literal; @i pub include $name:ident $(as $alias:ident)?) => {
        $crate::bake!($pre, $t, $suf; @m pub : include $name $(as $alias)?);

    };
    ($pre:literal, $t:path, $suf:literal; @i include $name:ident $(as $alias:ident)?) => {
        $crate::bake!($pre, $t, $suf; @m pub(super): include $name $(as $alias)?);
    };
    ($pre:literal, $t:path, $suf:literal; @m $($p:ident $(($c:ident))?)? : include $name:ident $(as $alias:ident)?) => {
        $($p $(($c))?)? use $name::$name $(as $alias)?;

        mod $name {

            static PARSE_CASH: std::sync::OnceLock<std::result::Result<<$t as $crate::Compileable>::Parsed, <$t as $crate::Compileable>::ParseError>> = std::sync::OnceLock::new();

            fn pc() -> &'static std::sync::OnceLock<std::result::Result<<$t as $crate::Compileable>::Parsed, <$t as $crate::Compileable>::ParseError>> {
                &PARSE_CASH
            }


            #[allow(non_upper_case_globals)]
            pub const $name: &$crate::Include<$t> = &$crate::Include::<$t> {
                code: include_str!(concat!($pre, stringify!($name), $suf),),
                parse_cash: pc,
                meta: $crate::Metadata {
                    language: <$t as $crate::Compileable>::NAME,
                    module: $crate::ModuleName(module_path!()),
                    file_path: $crate::FilePath(file!()),
                    name: stringify!($name)
                }
            };

            #[cfg(test)]
            mod test {
                #[test]
                fn validate() {
                    if let Err(e) = super::$name.parse() {
                        println!("{e:?}");
                        panic!("Invalid {} file", <$t as $crate::Compileable>::NAME);
                    }
                }
            }
        }
    };
    ($mod_name:ident; $pre:literal, $t:path, $suf:literal; {$($($i:ident)+ $(::$p:tt)* $(as $alias:ident)?;)*} $($rest:tt)*) => {
        pub mod $mod_name {
            $($crate::bake!($pre, $t, $suf; @i $($i)+ $(::$p)* $(as $alias)?);)*
            #[allow(non_snake_case)]

            pub fn ALL() -> impl Iterator<Item = &'static $crate::Include<$t>> {
                let x = std::iter::empty();
                $($crate::bake!($mod_name; x @glob $($i)+ $(::$p)* $(as $alias)?);)*
                x
            }
        }
    }
}

/// Something that can interpret an [Include] to
/// produce a Value or Error at runtime
pub trait Runner<L: Parseable> {
    type Context;
    type Output;
    type Error;

    fn run(ctx: Self::Context, inc: &'static Include<L>) -> Result<Self::Output, Self::Error>;
}

/// A include that provides some mechanism
/// to transform its content into another form
/// using additional context (e.g. a std library or imports)
pub trait Compileable {
    const NAME: &'static str;

    type Parsed: Send + Sync;
    type ParseError: std::fmt::Debug + Send + Sync;

    fn parse(inc: &Include<Self>) -> Result<Self::Parsed, Self::ParseError>;

    type Compiled: Send + Sync;
    type CompileError: std::fmt::Debug + Send + Sync;
    type Context<'a>
    where
        Self: 'a;

    fn compile(
        inc: &Include<Self>,
        ctx: &Self::Context<'_>,
    ) -> Result<Self::Compiled, Self::CompileError>;
}

impl<T: Parseable + ?Sized> Compileable for T {
    const NAME: &'static str = <T as Parseable>::NAME;

    type Parsed = T::Parsed;

    type ParseError = T::ParseError;

    fn parse(inc: &Include<Self>) -> Result<Self::Parsed, Self::ParseError> {
        <T as Parseable>::parse(inc)
    }

    type Compiled = T::Parsed;

    type CompileError = T::ParseError;

    type Context<'a>
        = ()
    where
        T: 'a;

    fn compile<'a>(
        inc: &'a Include<Self>,
        _ctx: &'a (),
    ) -> Result<Self::Compiled, Self::CompileError> {
        <T as Parseable>::parse(inc)
    }
}

/// An include that provides some mechanism
/// to verify the content of the included file
///
/// Calling `compile` on this has the same effect
/// as calling `parse`. For actually compilable
/// includes use [Compileable]
pub trait Parseable {
    type Parsed: Send + Sync;
    type ParseError: std::fmt::Debug + Send + Sync;

    const NAME: &'static str;

    fn parse(inc: &Include<Self>) -> Result<Self::Parsed, Self::ParseError>;
}

/// An include that only contains text
/// `parse()` will always succeed, simply returning
/// the underlying data as a `&'static str`
pub trait Data {
    const NAME: &'static str;
}

impl<T: Data> Parseable for T {
    type Parsed = &'static str;

    type ParseError = Infallible;

    const NAME: &'static str = <Self as Data>::NAME;

    fn parse(inc: &Include<Self>) -> Result<Self::Parsed, Self::ParseError> {
        Ok(inc.code)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug)]
pub struct Include<T: Compileable + ?Sized + 'static> {
    pub code: &'static str,
    pub meta: Metadata,
    #[allow(clippy::type_complexity)] // factoring this out is a real headache
    #[cfg_attr(feature = "serde", serde(skip))]
    pub parse_cash: fn() -> &'static OnceLock<Result<T::Parsed, T::ParseError>>,
}

impl<T: Compileable + 'static> Copy for Include<T> {}

impl<T: Compileable> Clone for Include<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy)]
pub struct Metadata {
    pub language: &'static str,
    pub module: ModuleName,
    pub file_path: FilePath,
    pub name: &'static str,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy)]
pub struct ModuleName(pub &'static str);

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy)]
pub struct FilePath(pub &'static str);

impl Metadata {
    pub const fn with_name(self, name: &'static str) -> Self {
        Self { name, ..self }
    }
}

impl<T: Compileable> Include<T> {
    pub const fn with_name(self, name: &'static str) -> Self {
        Self {
            meta: self.meta.with_name(name),
            ..self
        }
    }

    pub fn parse(&self) -> Result<&T::Parsed, &T::ParseError> {
        (self.parse_cash)().get_or_init(|| T::parse(self)).as_ref()
    }

    pub fn compile<'a>(&'a self, ctx: &'a T::Context<'a>) -> Result<T::Compiled, T::CompileError> {
        T::compile(self, ctx)
    }

    pub fn get_parse_error(&self) -> Option<&T::ParseError> {
        (self.parse_cash)()
            .get_or_init(|| T::parse(self))
            .as_ref()
            .err()
    }

    #[cfg(feature = "miette")]
    pub fn to_named_source(&self) -> miette::NamedSource<&'static str> {
        miette::NamedSource::new(
            std::path::Path::new(self.meta.file_path.0).to_string_lossy(),
            self.code,
        )
    }
}

#[macro_export]
macro_rules! def_lang {
    ($macro_name:ident; $lang_struct:path : $suf:literal) => {$crate::def_lang!($macro_name; "", $lang_struct, $suf, $);};
    ($macro_name:ident; $pre:literal, $lang_struct:path, $suf:literal) => {$crate::def_lang!($macro_name; $pre, $lang_struct, $suf, $);};
    ($macro_name:ident; $pre:literal, $lang_struct:path, $suf:literal, $dollar:tt) => {

        #[macro_export]
        macro_rules! $macro_name {
            ($dollar($dollar t:tt)*) => {
                $crate::bake!($macro_name; $pre, $lang_struct, $suf; {$dollar($dollar t)*});
            };
        }
    };
}
