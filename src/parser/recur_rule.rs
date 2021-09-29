use std::fmt;

#[cfg(feature = "arbitrary")]
use arbitrary::{Arbitrary, Result, Unstructured};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::opt,
    multi::{fold_many0, separated_list0},
    sequence::{preceded, tuple},
    IResult,
};

use crate::weekday::{weekday, Weekday};

use super::{
    chrono_utils::{enddate, RecurEnd},
    digits,
    frequency::{freq, Frequency},
    rrulparams, WeekdayNum,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RecurRule {
    pub(crate) freq: Frequency,
    pub(crate) end: RecurEnd,
    pub(crate) interval: u32,
    pub(crate) bysecond: Option<Vec<u8>>,
    pub(crate) byminute: Option<Vec<u8>>,
    pub(crate) byhour: Option<Vec<u8>>,
    pub(crate) byday: Option<Vec<WeekdayNum>>,
    pub(crate) bymonthday: Option<Vec<i8>>,
    pub(crate) byyearday: Option<Vec<i16>>,
    pub(crate) byweekno: Option<Vec<i8>>,
    pub(crate) bymonth: Option<Vec<u8>>,
    pub(crate) bysetpos: Option<Vec<i16>>,
    pub(crate) weekstart: Weekday,
}

#[cfg(feature = "arbitrary")]
use crate::parser::arbitrary_enums::Enum2;

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for RecurRule {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let freq = Frequency::arbitrary(u)?;
        let end = RecurEnd::arbitrary(u)?;
        let interval = u32::arbitrary(u)?;
        let bysecond = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let element = u.int_in_range(0_u8..=60_u8)?;
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let byminute = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let element = u.int_in_range(0_u8..=59_u8)?;
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let byhour = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let element = u.int_in_range(0_u8..=23_u8)?;
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let byday = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let ordwk = match u.choose(&[Enum2::A, Enum2::B])? {
                        Enum2::A => None,
                        Enum2::B => Some(u.int_in_range(-53_i8..=53_i8)?),
                    };
                    my_collection.push(WeekdayNum {
                        ordwk,
                        weekday: Weekday::arbitrary(u)?,
                    });
                }
                Some(my_collection)
            }
        };
        let bymonthday = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let mut element = u.int_in_range(-31..=30)?;
                    if element == 0 {
                        element = 31;
                    }
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let byyearday = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let mut element = u.int_in_range(-366..=365)?;
                    if element == 0 {
                        element = 366;
                    }
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let bymonth = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let element = u.int_in_range(1..=12)?;
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let bysetpos = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let mut element = u.int_in_range(-366..=365)?;
                    if element == 0 {
                        element = 366;
                    }
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let byweekno = match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => None,
            Enum2::B => {
                let len = u.arbitrary_len::<u8>()?;
                let mut my_collection = Vec::with_capacity(len);
                for _ in 0..len {
                    let mut element = u.int_in_range(-53..=52)?;
                    if element == 0 {
                        element = 53;
                    }
                    my_collection.push(element);
                }
                Some(my_collection)
            }
        };
        let weekstart = Weekday::arbitrary(u)?;

        Ok(RecurRule {
            freq,
            end,
            interval,
            bysecond,
            byminute,
            byhour,
            byday,
            bymonthday,
            byyearday,
            bymonth,
            bysetpos,
            byweekno,
            weekstart,
        })
    }
}

impl fmt::Display for RecurRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bysecondstring = match &self.bysecond {
            Some(v) => {
                ";BYSECOND=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let byminutestring = match &self.byminute {
            Some(v) => {
                ";BYMINUTE=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let byhourstring = match &self.byhour {
            Some(v) => {
                ";BYHOUR=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let bydaystring = match &self.byday {
            Some(v) => ";BYDAY=".to_string() + &v.iter().map(|v| match v.ordwk {
                Some(q) => q.to_string(),
                None => "".to_string(),
            } + &v.weekday.to_string()).collect::<Vec<String>>().join(","),
            None => "".to_string(),
        };
        let bymonthdaystring = match &self.bymonthday {
            Some(v) => {
                ";BYMONTHDAY=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let byyeardaystring = match &self.byyearday {
            Some(v) => {
                ";BYYEARDAY=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let byweeknostring = match &self.byweekno {
            Some(v) => {
                ";BYWEEKNO=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let bymonthstring = match &self.bymonth {
            Some(v) => {
                ";BYMONTH=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let bysetposstring = match &self.bysetpos {
            Some(v) => {
                ";BYSETPOS=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
            }
            None => "".to_string(),
        };
        let intervalstring = if self.interval == 1 {
            "".to_string()
        } else {
            ";INTERVAL=".to_string() + &self.interval.to_string()
        };
        let weekstartstring = if self.weekstart == Weekday::Mon {
            "".to_string()
        } else {
            ";WKST=".to_string() + &self.weekstart.to_string()
        };
        write!(
            f,
            "RRULE:FREQ={}{}{}{}{}{}{}{}{}{}{}{}{}",
            self.freq,
            intervalstring,
            self.end,
            bysecondstring,
            byhourstring,
            byminutestring,
            bymonthstring,
            weekstartstring,
            byweeknostring,
            bydaystring,
            bymonthdaystring,
            byyeardaystring,
            bysetposstring,
        )
    }
}

impl Default for RecurRule {
    fn default() -> Self {
        RecurRule {
            freq: Frequency::Yearly, // TODO FIXME no default
            end: RecurEnd::Forever,
            interval: 1,
            bysecond: None,
            byminute: None,
            byhour: None,
            byday: None,
            bymonthday: None,
            byyearday: None,
            byweekno: None,
            bymonth: None,
            bysetpos: None,
            weekstart: Weekday::Mon,
        }
    }
}

enum RecurRulePart {
    Freq(Frequency),
    End(RecurEnd),
    Interval(u32),
    Bysecond(Vec<u8>),
    Byminute(Vec<u8>),
    Byhour(Vec<u8>),
    Byday(Vec<WeekdayNum>),
    Bymonthday(Vec<i8>),
    Byyearday(Vec<i16>),
    Byweekno(Vec<i8>),
    Bymonth(Vec<u8>),
    Bysetpos(Vec<i16>),
    Weekstart(Weekday),
}

fn recur_rule_part(input: &str) -> IResult<&str, RecurRulePart> {
    alt((
        nom::combinator::map(preceded(tag("FREQ="), freq), RecurRulePart::Freq),
        nom::combinator::map(preceded(tag("UNTIL="), enddate), |v| {
            RecurRulePart::End(RecurEnd::Until(v))
        }),
        nom::combinator::map(preceded(tag("COUNT="), digits(1..)), |v| {
            RecurRulePart::End(RecurEnd::Count(v))
        }),
        nom::combinator::map(
            preceded(tag("INTERVAL="), digits(1..)),
            RecurRulePart::Interval,
        ),
        nom::combinator::map(
            preceded(tag("BYSECOND="), separated_list0(tag(","), digits(0..=60))),
            RecurRulePart::Bysecond,
        ),
        nom::combinator::map(
            preceded(tag("BYMINUTE="), separated_list0(tag(","), digits(0..=59))),
            RecurRulePart::Byminute,
        ),
        nom::combinator::map(
            preceded(tag("BYHOUR="), separated_list0(tag(","), digits(0..=23))),
            RecurRulePart::Byhour,
        ),
        nom::combinator::map(
            preceded(
                tag("BYDAY="),
                separated_list0(
                    tag(","),
                    nom::combinator::map(tuple((opt(digits((-53)..=53)), weekday)), |v| {
                        WeekdayNum {
                            ordwk: v.0,
                            weekday: v.1,
                        }
                    }),
                ),
            ),
            RecurRulePart::Byday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYMONTHDAY="),
                separated_list0(tag(","), digits(-31..=31)),
            ),
            RecurRulePart::Bymonthday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYYEARDAY="),
                separated_list0(tag(","), digits(-366..=366)),
            ),
            RecurRulePart::Byyearday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYWEEKNO="),
                separated_list0(tag(","), digits(-53..=53)),
            ),
            RecurRulePart::Byweekno,
        ),
        nom::combinator::map(
            preceded(tag("BYMONTH="), separated_list0(tag(","), digits(1..=12))),
            RecurRulePart::Bymonth,
        ),
        nom::combinator::map(
            preceded(
                tag("BYSETPOS="),
                separated_list0(tag(","), digits(-366..=366)),
            ),
            RecurRulePart::Bysetpos,
        ),
        nom::combinator::map(preceded(tag("WKST="), weekday), RecurRulePart::Weekstart),
    ))(input)
}

pub fn recur(input: &str) -> IResult<&str, RecurRule> {
    // frequency is required and has to be first.
    let (input, freq) = preceded(tag("FREQ="), freq)(input)?;

    let x = fold_many0(
        preceded(tag(";"), recur_rule_part),
        || RecurRule {
            freq,
            ..Default::default()
        },
        |mut acc, item| {
            // TODO FIXME detect overrides (which is currently not possible for alles fields like this)
            match item {
                RecurRulePart::Freq(_v) => acc.freq = freq,
                RecurRulePart::End(v) => acc.end = v,
                RecurRulePart::Interval(v) => acc.interval = v,
                RecurRulePart::Bysecond(v) => acc.bysecond = Some(v),
                RecurRulePart::Byminute(v) => acc.byminute = Some(v),
                RecurRulePart::Byhour(v) => acc.byhour = Some(v),
                RecurRulePart::Byday(v) => acc.byday = Some(v),
                RecurRulePart::Bymonthday(v) => acc.bymonthday = Some(v),
                RecurRulePart::Byyearday(v) => acc.byyearday = Some(v),
                RecurRulePart::Byweekno(v) => acc.byweekno = Some(v),
                RecurRulePart::Bymonth(v) => acc.bymonth = Some(v),
                RecurRulePart::Bysetpos(v) => acc.bysetpos = Some(v),
                RecurRulePart::Weekstart(v) => acc.weekstart = v,
            };
            acc
        },
    )(input);
    let _ = x;
    x
}

// but it SHOULD NOT be specified more than once. The recurrence set generated with multiple "RRULE" properties is undefined.
pub fn rrule(input: &str) -> IResult<&str, RecurRule> {
    let (input, _params) = preceded(tag("RRULE"), rrulparams)(input)?;
    // TODO FIXME don't drop params
    preceded(tag(":"), recur)(input)
}
