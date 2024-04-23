#include <ranges>



template <std::ranges::view range_t>
struct drop_view : public std::ranges::view_interface<drop_view<range_t>>
{
public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    constexpr drop_view() noexcept
        requires std::default_initializable<range_t> = default;

    constexpr drop_view(range_t range, difference_type count) noexcept
        : m_range{std::move(range)}, m_count{std::move(count)} {}


public:
    [[nodiscard]]
    constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t>
    {
        constexpr auto size  = std::ranges::size(m_range);


        return size < std::ranges::range_size_t<range_t>(count) 
                                       ? std::ranges::range_size_t<range_t>(0)
                                       : size - std::ranges::range_size_t<range_t>(m_count);
    }

    [[nodiscard]]
    constexpr auto size() const noexcept -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t>
    {
        using size_type = std::ranges::range_size_t<const range_t>;

        constexpr auto size  = std::ranges::size(m_range);


        return size < std::ranges::range_size_t<const range_t>(count) 
                                       ? std::ranges::range_size_t<const range_t>(0)
                                       : size - std::ranges::range_size_t<const range_t>(m_count);
    }

public:
    constexpr auto begin() noexcept
    {
        return std::ranges::next(
                std::ranges::begin(m_range), m_count, std::ranges::end(m_range)
            );
    }

    constexpr auto begin() const noexcept
    {
        return std::make_const_iterator(
                    std::ranges::next(
                        std::ranges::begin(m_range), m_count, std::ranges::end(m_range)
                    )
                );
    }

    constexpr auto end()       noexcept { return std::ranges::end(m_range); }
    constexpr auto end() const noexcept
    {
        return std::make_const_sentinel(std::ranges::end(m_range));
    }


private:
    range_t         m_range = range_t{};
    difference_type m_count = 0UL;
};

template <typename range_t>
drop_view(range_t&&, std::ranges::range_difference_t<range_t>)
    -> drop_view<std::views::all_t<range_t>>;


template <typename range_t>
inline constexpr auto std::ranges::enable_borrowed_range<drop_view<range_t>>
    = std::ranges::enable_borrowed_range<range_t>;



template <std::integral Int>
struct drop_view_closure
    : public std::ranges::range_adaptor_closure<drop_view_closure<Int>>
{
public:
    constexpr drop_view_closure(Int count) noexcept : m_count{ count } {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& view) const noexcept
    {
        return drop_view{ std::forward<range_t>(view), m_count };
    }

private:
    Int m_count;
};


struct drop_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
                typename difference_t = std::ranges::range_difference_t<range_t>>
    constexpr auto operator()(range_t&& view, difference_t count) const noexcept
    {
        return drop_view{ std::forward<range_t>(view), count };
    }

    constexpr auto operator()(std::integral auto count) const noexcept
    {
        return drop_view_closure{ count };
    }
};

namespace views {
    inline constexpr drop_view_adaptor drop{};
}