export const plainDateTimeImpl = (just) => (nothing) => (f) => {
  try {
    return just(new Temporal.PlainDateTime(
      f.year, f.month, f.day,
      f.hour, f.minute, f.second,
      f.millisecond, f.microsecond, f.nanosecond
    ));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.PlainDateTime.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getYear = (dt) => dt.year;
export const getMonth = (dt) => dt.month;
export const getDay = (dt) => dt.day;
export const getHour = (dt) => dt.hour;
export const getMinute = (dt) => dt.minute;
export const getSecond = (dt) => dt.second;
export const getMillisecond = (dt) => dt.millisecond;
export const getMicrosecond = (dt) => dt.microsecond;
export const getNanosecond = (dt) => dt.nanosecond;
export const getDayOfWeek = (dt) => dt.dayOfWeek;
export const getDayOfYear = (dt) => dt.dayOfYear;
export const getWeekOfYear = (dt) => dt.weekOfYear;
export const getYearOfWeek = (dt) => dt.yearOfWeek;
export const getDaysInMonth = (dt) => dt.daysInMonth;
export const getDaysInWeek = (dt) => dt.daysInWeek;
export const getDaysInYear = (dt) => dt.daysInYear;
export const getMonthsInYear = (dt) => dt.monthsInYear;
export const getInLeapYear = (dt) => dt.inLeapYear;

export const withImpl = (just) => (nothing) => (fields) => (pdt) => {
  try {
    return just(pdt.with(fields));
  } catch (_) {
    return nothing;
  }
};

export const withPlainTime = (time) => (pdt) => pdt.withPlainTime(time);

export const addImpl = (just) => (nothing) => (dur) => (pdt) => {
  try {
    return just(pdt.add(dur));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (dur) => (pdt) => {
  try {
    return just(pdt.subtract(dur));
  } catch (_) {
    return nothing;
  }
};

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);
export const roundImpl = (opts) => (pdt) => pdt.round(opts);

export const toPlainDate = (dt) => dt.toPlainDate();
export const toPlainTime = (dt) => dt.toPlainTime();
export const toPlainYearMonth = (dt) => dt.toPlainYearMonth();
export const toPlainMonthDay = (dt) => dt.toPlainMonthDay();
export const toZonedDateTimeImpl = (tz) => (disambiguation) => (pdt) =>
  pdt.toZonedDateTime(tz, { disambiguation });

export const toString = (dt) => dt.toString();
