use std::num::NonZeroI64;

use chrono::{DateTime, Datelike, Duration, Timelike, Utc};

use crate::parser::{
    chrono_utils::{date_or_datetime_to_utc, RRuleDateOrDateTime},
    recur_rule::RecurRule,
};

pub struct RRule {
    rrule: RecurRule,
    dtstart: RRuleDateOrDateTime,
}
/*
impl Iterator for RecurRule {
    type Item = DateTime<Utc>;

    fn next(&mut self) -> Option<Self::Item> {

    }
}
*/

#[derive(Clone, Copy)]
pub struct MaybeInvalidDateTime {
    second: u32,
    minute: u32,
    hour: u32,
    day: u32,
    month0: u32,
    year: i32,
}

impl From<DateTime<Utc>> for MaybeInvalidDateTime {
    fn from(datetime: DateTime<Utc>) -> MaybeInvalidDateTime {
        MaybeInvalidDateTime {
            second: datetime.second(),
            minute: datetime.minute(),
            hour: datetime.hour(),
            day: datetime.day(),
            // weeks
            month0: datetime.month0(),
            year: datetime.year(),
        }
    }
}

pub fn add_month(mut datetime: MaybeInvalidDateTime, interval: u32) -> MaybeInvalidDateTime {
    datetime.month0 = (datetime.month0 + interval) % 12;
    datetime.year += interval as i32 / 12_i32;
    datetime
}

pub fn add_year(mut datetime: MaybeInvalidDateTime, interval: i32) -> MaybeInvalidDateTime {
    datetime.year = datetime.year + interval;
    datetime
}

pub fn iterate(rrule: RRule /*, rdate: &str, exdate: &str*/) -> MaybeInvalidDateTime {
    let dtstart = date_or_datetime_to_utc(rrule.dtstart);

    let test: NonZeroI64 = rrule.rrule.interval.into();
    let test2: u32 = rrule.rrule.interval.into();

    match rrule.rrule.freq {
        crate::parser::frequency::Frequency::Secondly => {
            (dtstart + Duration::seconds(test.into())).into()
        }
        crate::parser::frequency::Frequency::Minutely => {
            (dtstart + Duration::minutes(test.into())).into()
        }
        crate::parser::frequency::Frequency::Hourly => {
            (dtstart + Duration::hours(test.into())).into()
        }
        crate::parser::frequency::Frequency::Daily => {
            (dtstart + Duration::days(test.into())).into()
        }
        crate::parser::frequency::Frequency::Weekly => {
            (dtstart + Duration::weeks(test.into())).into()
        }
        crate::parser::frequency::Frequency::Monthly => add_month(dtstart.into(), rrule.rrule.interval.into()), // not easy maybe month doesnt have that day
        crate::parser::frequency::Frequency::Yearly => add_year(dtstart.into(), test2 as i32), //dtstart + Duration::years(test.into()), // not easy february is an asshole
                                                                                 // even worse as when by* is used this could probably create valid shit?
    }
}
