use std::num::NonZeroI64;

use chrono::{DateTime, Datelike, Duration, NaiveDate, Timelike, Utc};

use crate::parser::recur_rule::RecurRule;

pub struct RRule {
    pub(crate) rrule: RecurRule,
    pub(crate) dtstart: MaybeInvalidDateTime,
}

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

impl From<MaybeInvalidDateTime> for DateTime<Utc> {
    fn from(datetime: MaybeInvalidDateTime) -> DateTime<Utc> {
        DateTime::from_utc(
            NaiveDate::from_ymd(datetime.year, datetime.month0 + 1, datetime.day).and_hms(
                datetime.hour,
                datetime.minute,
                datetime.second,
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
    datetime.year += interval;
    datetime
}

impl Iterator for RRule {
    type Item = MaybeInvalidDateTime;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO FIXME honor until/count
        //let dtstart = date_or_datetime_to_utc(rrule.dtstart);

        let test: NonZeroI64 = self.rrule.interval.into();
        let test2: u32 = self.rrule.interval.into();

        let new = match self.rrule.freq {
            crate::parser::frequency::Frequency::Secondly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::seconds(test.into())).into()
            }
            crate::parser::frequency::Frequency::Minutely => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::minutes(test.into())).into()
            }
            crate::parser::frequency::Frequency::Hourly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::hours(test.into())).into()
            }
            crate::parser::frequency::Frequency::Daily => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::days(test.into())).into()
            }
            crate::parser::frequency::Frequency::Weekly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::weeks(test.into())).into()
            }
            crate::parser::frequency::Frequency::Monthly => {
                add_month(self.dtstart, self.rrule.interval.into())
            } // not easy maybe month doesnt have that day
            crate::parser::frequency::Frequency::Yearly => add_year(self.dtstart, test2 as i32), //dtstart + Duration::years(test.into()), // not easy february is an asshole
                                                                                                 // even worse as when by* is used this could probably create valid shit?
        };
        self.dtstart = new;

        Some(new)
    }
}
