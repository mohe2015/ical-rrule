use nom::{IResult, bytes::complete::{is_not, tag}, multi::many0};

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

