#include <ranges>
#include <sstream>


template <typename CharT>
struct join_view_closure
    : public std::ranges::range_adaptor_closure<join_view_closure<CharT>>
{
public:
    constexpr join_view_closure(CharT seprator) noexcept
        : m_separator{std::move(seprator)}
    {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t& range) const noexcept -> std::string
    {
        if (std::ranges::empty(range)) return "";

        std::ostringstream output{};

        auto begin = std::ranges::begin(range);

        output << *begin;

        begin = std::ranges::next(std::move(begin));

        while (begin != std::ranges::end(range))
        {
            output << m_separator << *begin;
            begin = std::ranges::next(std::move(begin));
        }

        return output.str();
    }

    template <std::ranges::bidirectional_range range_t>
    constexpr auto operator()(range_t&& range) const noexcept -> std::string
    {
        if (std::ranges::empty(range)) return "";

        using value_t = std::ranges::range_value_t<range_t>;

        std::ostringstream output{};

        auto begin = std::ranges::begin(range);
        auto end   = std::ranges::end(range);

        std::ranges::copy(begin, std::ranges::prev(end),
                          std::ostream_iterator<value_t>(output, m_separator));

        begin = std::ranges::prev(end);

        if (begin != end) output << *begin;

        return output.str();
    }

private:
    CharT m_separator;
};


struct join_view_adaptor
{
public:
    template <typename CharT>
    constexpr auto operator()(CharT seprator) const noexcept
    {
        return join_view_closure{std::move(seprator)};
    }
};

namespace views {
    inline constexpr join_view_adaptor join{};
}
