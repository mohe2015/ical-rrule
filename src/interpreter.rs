use std::num::NonZeroI64;

use chrono::{DateTime, Datelike, Duration, NaiveDate, Timelike, Utc};

use crate::parser::{
    chrono_utils::{date_or_datetime_to_utc, RRuleDateOrDateTime},
    recur_rule::RecurRule,
};

pub struct RRule {
    rrule: RecurRule,
    dtstart: MaybeInvalidDateTime,
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

impl Into<DateTime<Utc>> for MaybeInvalidDateTime {
    fn into(self: MaybeInvalidDateTime) -> DateTime<Utc> {
        DateTime::from_utc(
            NaiveDate::from_ymd(self.year, self.month0 + 1, self.day).and_hms(
                self.hour,
                self.minute,
                self.second,
            ),
            Utc,
        )
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

impl Iterator for RRule {
    type Item = MaybeInvalidDateTime;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO FIXME honor until/count
        //let dtstart = date_or_datetime_to_utc(rrule.dtstart);

        let test: NonZeroI64 = self.rrule.interval.into();
        let test2: u32 = self.rrule.interval.into();

        match self.rrule.freq {
            crate::parser::frequency::Frequency::Secondly => {
                Some((Into::<DateTime<Utc>>::into(self.dtstart) + Duration::seconds(test.into())).into())
            }
            crate::parser::frequency::Frequency::Minutely => {
                Some((Into::<DateTime<Utc>>::into(self.dtstart) + Duration::minutes(test.into())).into())
            }
            crate::parser::frequency::Frequency::Hourly => {
                Some((Into::<DateTime<Utc>>::into(self.dtstart) + Duration::hours(test.into())).into())
            }
            crate::parser::frequency::Frequency::Daily => {
                Some((Into::<DateTime<Utc>>::into(self.dtstart) + Duration::days(test.into())).into())
            }
            crate::parser::frequency::Frequency::Weekly => {
                Some((Into::<DateTime<Utc>>::into(self.dtstart) + Duration::weeks(test.into())).into())
            }
            crate::parser::frequency::Frequency::Monthly => {
                Some(add_month(self.dtstart, self.rrule.interval.into()))
            } // not easy maybe month doesnt have that day
            crate::parser::frequency::Frequency::Yearly => Some(add_year(self.dtstart, test2 as i32)), //dtstart + Duration::years(test.into()), // not easy february is an asshole
                                                                                                 // even worse as when by* is used this could probably create valid shit?
        }
    }
}
