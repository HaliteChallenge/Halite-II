use std::str::FromStr;
use std::fmt::Debug;

pub trait Decodable {
    fn parse<'a, I>(&mut I) -> Self
    where
        I: Iterator<Item = &'a str>;
}

fn parse_next_primitive<'a, T: FromStr, I>(tokens: &mut I, type_name: &str) -> T
where
    I: Iterator<Item = &'a str>,
    <T as FromStr>::Err: Debug,
{
    let next_token = tokens.next().expect("Expected token, got EOF");
    next_token.parse::<T>().unwrap_or_else(|_| panic!(
        "Could not parse {:?} as expected type {:?}",
        next_token,
        type_name
    ))
}

impl Decodable for f64 {
    fn parse<'a, I>(tokens: &mut I) -> f64
    where
        I: Iterator<Item = &'a str>,
    {
        parse_next_primitive(tokens, "f64")
    }
}

impl Decodable for i32 {
    fn parse<'a, I>(tokens: &mut I) -> i32
    where
        I: Iterator<Item = &'a str>,
    {
        parse_next_primitive(tokens, "i32")
    }
}

impl Decodable for usize {
    fn parse<'a, I>(tokens: &mut I) -> usize
    where
        I: Iterator<Item = &'a str>,
    {
        parse_next_primitive(tokens, "usize")
    }
}

impl Decodable for bool {
    fn parse<'a, I>(tokens: &mut I) -> bool
    where
        I: Iterator<Item = &'a str>,
    {
        let p = parse_next_primitive(tokens, "bool");
        match p {
            0 => false,
            1 => true,
            _ => panic!("Expected 0 or 1, got {:?}", p),
        }
    }
}

impl<T: Decodable> Decodable for Box<[T]> {
    fn parse<'a, I>(tokens: &mut I) -> Box<[T]>
    where
        I: Iterator<Item = &'a str>,
    {
        let size: i64 = parse_next_primitive(tokens, "i64");
        let mut result = Vec::with_capacity(size as usize);
        for _ in 0..size {
            let p = T::parse(tokens);
            result.push(p);
        }
        result.into_boxed_slice()
    }
}

impl<T: Decodable> Decodable for Option<T> {
    fn parse<'a, I>(tokens: &mut I) -> Option<T>
    where
        I: Iterator<Item = &'a str>,
    {
        let present = bool::parse(tokens);
        let value = T::parse(tokens);

        match present {
            true => Some(value),
            false => None,
        }
    }
}
