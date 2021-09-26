use std::num::NonZeroI64;

use chrono::{DateTime, Duration, Utc};

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

pub fn iterate(rrule: RRule /*, rdate: &str, exdate: &str*/) -> DateTime<Utc> {
    let dtstart = date_or_datetime_to_utc(rrule.dtstart);

    let test: NonZeroI64 = rrule.rrule.interval.into();

    match rrule.rrule.freq {
        crate::parser::frequency::Frequency::Secondly => dtstart + Duration::seconds(test.into()),
        crate::parser::frequency::Frequency::Minutely => dtstart + Duration::minutes(test.into()),
        crate::parser::frequency::Frequency::Hourly => dtstart + Duration::hours(test.into()),
        crate::parser::frequency::Frequency::Daily => dtstart + Duration::days(test.into()),
        crate::parser::frequency::Frequency::Weekly => dtstart + Duration::weeks(test.into()),
        crate::parser::frequency::Frequency::Monthly => todo!(), //dtstart + Duration::month(test.into()), // not easy maybe month doesnt have that day
        crate::parser::frequency::Frequency::Yearly => todo!(), //dtstart + Duration::years(test.into()), // not easy february is an asshole
    }
}
