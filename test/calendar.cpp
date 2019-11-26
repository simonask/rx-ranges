#include <boost/date_time/gregorian/gregorian.hpp>
#include <iostream>
#include <rx/ranges.hpp>
#include <utility>


using namespace RX_NAMESPACE;

// This example is derived from the 'rangeless' library's example.
//
// See: https://github.com/ast-al/rangeless/blob/gh-pages/test/calendar.cpp

namespace greg = boost::gregorian;
using date_t = greg::date;

static void make_calendar(uint16_t year, uint8_t num_months_horizontally, std::ostream& os) {
    static constexpr std::array s_month_names = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    seq(date_t(year, greg::Jan, 1), greg::date_duration(1))
        | until([year](date_t x) { return x.year() != year; })
        | group_adjacent_by(
            [](const date_t& d) { return std::make_pair(d.month(), d.week_number()); })
        | transform([&](const auto &wk_dates) -> std::pair<date_t::month_type, std::string> {
              const auto left_pad_amt = size_t(3 * ((wk_dates.get().day_of_week() + 7 - 1) % 7));
              return {wk_dates.get().month(),
                      wk_dates
                          | foldl(
                              std::string(left_pad_amt, ' '),
                              [](std::string ret_wk, const date_t& d) {
                                  return std::move(ret_wk) + (d.day() < 10 ? "  " : " ")
                                         + std::to_string(d.day());
                              })};
          })                                                       //
        | group_adjacent_by([](const auto& x) { return x.first; }) // group by month
        | in_groups_of(num_months_horizontally)                    //
        | for_each([&](const auto& group) {
              for (int row = -2; row < 6; ++row) {
                  for (const auto& mo : group | transform(to_vector())) {
                      os << std::setiosflags(std::ios::left) << std::setw(25)
                         << (row == -2 ? std::string{"        "} + s_month_names.at(mo.front().first - 1u)
                                       : row == -1 ? " Mo Tu We Th Fr Sa Su"
                                                   : size_t(row) < mo.size()
                                                         ? mo[row].second // formatted week
                                                         : "                     ");
                  }
                  os << std::endl;
              }
          });
}

int main() {
    make_calendar(2019, 3, std::cout);
    return 0;
}
