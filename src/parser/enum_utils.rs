use nom::{bytes::complete::tag, IResult};

pub const fn enum_element<'a, T: Copy>(
    string: &'static str,
    enum_element: T,
) -> impl FnMut(&'a str) -> IResult<&'a str, T> {
    move |input| {
        let (input, _) = tag(string)(input)?;
        Ok((input, enum_element))
    }
}
