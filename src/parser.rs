use nom::{IResult, bytes::complete::tag};

pub fn parse(input: &[u8]) -> IResult<&[u8],&[u8]> {
    let (input, tag) = tag("RRULE")(input)?;

    Ok((input, tag))
}



#[cfg(test)]
mod tests {
    use nom::{IResult, error::ErrorKind};

    use crate::parser::parse;

    #[test]
    fn it_works() {        
        assert_eq!(parse(&b"RRULE"[..]), IResult::Ok((&b""[..], &b"RRULE"[..])));
        assert_eq!(parse(&b"NOTRRULE"[..]), Err(nom::Err::Error(nom::error::Error { input: &b"NOTRRULE"[..], code: ErrorKind::Tag })));
    }
}

