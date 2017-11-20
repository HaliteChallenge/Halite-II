
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
    let next_token = &tokens.next().expect(&"Expected token, got EOF");
    let value: T = next_token.parse().expect(&format!(
        "Could not parse {:?} as expected type {:?}",
        &next_token,
        type_name
    ));
    return value;
}

impl Decodable for f64 {
    fn parse<'a, I>(tokens: &mut I) -> f64
    where
        I: Iterator<Item = &'a str>,
    {
        return parse_next_primitive(tokens, "f64");
    }
}

impl Decodable for i32 {
    fn parse<'a, I>(tokens: &mut I) -> i32
    where
        I: Iterator<Item = &'a str>,
    {
        return parse_next_primitive(tokens, "i32");
    }
}

impl Decodable for usize {
    fn parse<'a, I>(tokens: &mut I) -> usize
    where
        I: Iterator<Item = &'a str>,
    {
        return parse_next_primitive(tokens, "usize");
    }
}

impl Decodable for bool {
    fn parse<'a, I>(tokens: &mut I) -> bool
    where
        I: Iterator<Item = &'a str>,
    {
        let p = parse_next_primitive(tokens, "bool");
        return match p {
            0 => false,
            1 => true,
            _ => panic!(format!("Expected 0 or 1, got {:?}", p)),
        };
    }
}

impl<T: Decodable> Decodable for Vec<T> {
    fn parse<'a, I>(tokens: &mut I) -> Vec<T>
    where
        I: Iterator<Item = &'a str>,
    {
        let size = parse_next_primitive(tokens, "i64");
        let mut result = Vec::with_capacity(size);
        for _ in 0..size {
            let p = T::parse(tokens);
            result.push(p);
        }
        return result;
    }
}

impl<T: Decodable> Decodable for Option<T> {
    fn parse<'a, I>(tokens: &mut I) -> Option<T>
    where
        I: Iterator<Item = &'a str>,
    {

        let present = bool::parse(tokens);
        let value = T::parse(tokens);

        return match present {
            true => Some(value),
            false => None,
        };
    }
}