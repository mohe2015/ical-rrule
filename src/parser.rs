use nom::{IResult, bytes::complete::{is_not, tag, take_while1}, character::is_alphanumeric, multi::many0};

pub fn constant_rrule(input: &str) -> IResult<&str, &str> {
    tag("RRULE")(input)
}

pub fn rrulparam(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag(";")(input)?;
    is_not(";:")(input)
    // TODO FIXME parse "other_param ="
}

pub fn rrulparams(input: &str) -> IResult<&str, Vec<&str>> {
    many0(rrulparam)(input) // TODO FIXME parse "other_param ="
}

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
pub fn other_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    iana_param(input)
    // TODO FIXME maybe support x_param
}

pub fn iana_token(input: &str) -> IResult<&str, &str> {
    // https://datatracker.ietf.org/doc/html/rfc5234
    take_while1(|c: char| c.is_ascii_alphanumeric() || c == '-')(input)
}

pub fn param_value(input: &str) -> IResult<&str, &str> {
    
}

pub fn iana_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, iana_token) = iana_token(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, param_values) = many0(param_value)(input)?;
    Ok((input, (iana_token, param_values)))
}

#[cfg(test)]
mod tests {
    use nom::{IResult, error::ErrorKind};

    use crate::parser::{constant_rrule, rrulparam, rrulparams};

    #[test]
    fn it_works() {        
        assert_eq!(constant_rrule("RRULE"), IResult::Ok(("", "RRULE")));
        assert_eq!(constant_rrule("NOTRRULE"), Err(nom::Err::Error(nom::error::Error { input: "NOTRRULE", code: ErrorKind::Tag })));
        assert_eq!(constant_rrule("RRULEEXTENDED"), IResult::Ok(("EXTENDED", "RRULE")));

        assert_eq!(rrulparam(";test"), IResult::Ok(("", "test")));
        assert_eq!(rrulparam(""), Err(nom::Err::Error(nom::error::Error { input: "", code: ErrorKind::Tag })));
        assert_eq!(rrulparam(";test;"), IResult::Ok((";", "test")));

        assert_eq!(rrulparams(""), IResult::Ok(("", vec![])));
        assert_eq!(rrulparams("nothing"), IResult::Ok(("nothing", vec![])));
        assert_eq!(rrulparams(";test"), IResult::Ok(("", vec!["test"])));
        assert_eq!(rrulparams(";test;test2"), IResult::Ok(("", vec!["test", "test2"])));
        assert_eq!(rrulparams(";test;test2;"), IResult::Ok((";", vec!["test", "test2"])));
    }
}

