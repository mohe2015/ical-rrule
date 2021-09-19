use nom::{
    bytes::complete::{tag, take_till, take_while1},
    multi::{many0, separated_list1},
    IResult,
};

pub fn constant_rrule(input: &str) -> IResult<&str, &str> {
    tag("RRULE")(input)
}

pub fn iana_token(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_ascii_alphanumeric() || c == '-')(input)
}

pub fn paramtext(input: &str) -> IResult<&str, &str> {
    take_till(|c: char| {
        (char::from(0x00) <= c && c <= char::from(0x08))
            || (char::from(0x0a) <= c && c <= char::from(0x1f))
            || c == char::from(0x7f)
            || c == ';'
            || c == ':'
            || c == ','
            || c == '"'
    })(input)
}

pub fn param_value(input: &str) -> IResult<&str, &str> {
    paramtext(input)
    // TODO FIXME
    //nom::branch::alt((paramtext, quoted_string))(input)
}

pub fn iana_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, iana_token) = iana_token(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, param_values) = separated_list1(tag(","), param_value)(input).unwrap(); // shouldn't be able to panic as param_value can be empty. will need to fuzz.
    Ok((input, (iana_token, param_values)))
}

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
pub fn other_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    iana_param(input)
    // TODO FIXME maybe support x_param
}

pub fn rrulparam(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, _) = tag(";")(input)?;
    other_param(input)
}

pub fn rrulparams(input: &str) -> IResult<&str, Vec<(&str, Vec<&str>)>> {
    many0(rrulparam)(input) // TODO FIXME parse "other_param ="
}

//pub fn quoted_string(input: &str) -> IResult<&str, &str> {}

#[cfg(test)]
mod tests {
    use nom::{error::ErrorKind, IResult};

    use crate::parser::{
        constant_rrule, iana_param, iana_token, other_param, param_value, paramtext, rrulparams,
    };

    #[test]
    fn it_works() {
        assert_eq!(constant_rrule("RRULE"), IResult::Ok(("", "RRULE")));
        assert_eq!(
            constant_rrule("NOTRRULE"),
            Err(nom::Err::Error(nom::error::Error {
                input: "NOTRRULE",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            constant_rrule("RRULEEXTENDED"),
            IResult::Ok(("EXTENDED", "RRULE"))
        );

        assert_eq!(
            iana_token(""),
            Err(nom::Err::Error(nom::error::Error {
                input: "",
                code: ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(
            iana_token("_"),
            Err(nom::Err::Error(nom::error::Error {
                input: "_",
                code: ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(iana_token("5"), IResult::Ok(("", "5")));
        assert_eq!(iana_token("i"), IResult::Ok(("", "i")));
        assert_eq!(
            iana_token("133THIS-IS-a-010-IAnATokn"),
            IResult::Ok(("", "133THIS-IS-a-010-IAnATokn"))
        );

        for fun in [paramtext, param_value] {
            assert_eq!(fun(""), IResult::Ok(("", "")));
            assert_eq!(fun("ä"), IResult::Ok(("", "ä")));
            assert_eq!(fun("耳ä"), IResult::Ok(("", "耳ä")));
            assert_eq!(fun("\""), IResult::Ok(("\"", "")));
        }

        for fun in [iana_param, other_param] {
            assert_eq!(fun("TEST=1"), IResult::Ok(("", ("TEST", vec!["1"]))));
            assert_eq!(
                fun("1-tEST=ädf,üsldifh"),
                IResult::Ok(("", ("1-tEST", vec!["ädf", "üsldifh"])))
            );
            assert_eq!(fun("TEST="), IResult::Ok(("", ("TEST", vec![""]))));
            assert_eq!(
                fun("TEST=,,1,,"),
                IResult::Ok(("", ("TEST", vec!["", "", "1", "", ""])))
            );
            assert_eq!(
                fun("TEST"),
                Err(nom::Err::Error(nom::error::Error {
                    input: "",
                    code: ErrorKind::Tag
                }))
            );
        }

        assert_eq!(rrulparams(""), IResult::Ok(("", vec![])));
        assert_eq!(
            rrulparams(";TEST=1"),
            IResult::Ok(("", vec![("TEST", vec!["1"])]))
        );
        assert_eq!(
            rrulparams(";TEST=1,2;TEST=3,4"),
            IResult::Ok(("", vec![("TEST", vec!["1", "2"]), ("TEST", vec!["3", "4"])]))
        );
        assert_eq!(rrulparams(";"), IResult::Ok((";", vec![])));
    }
}
