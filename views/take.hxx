#include <ranges>



template <std::ranges::view range_t>
struct take_view : public std::ranges::view_interface<take_view<range_t>>
{
public:
    struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    constexpr take_view() noexcept
        requires std::default_initializable<range_t> = default;

    constexpr take_view(range_t range, difference_type count) noexcept
        : m_range{std::move(range)}
        , m_count{std::move(count)}
    {}


public:
    constexpr auto size() noexcept requires( std::ranges::sized_range<range_t> )
    {
        return std::min(std::ranges::size(m_range),
                        std::ranges::range_size_t<range_t>(m_count));
    }

    constexpr auto size() const noexcept requires( std::ranges::sized_range<const range_t> )
    {
        return std::ranges::min(std::ranges::size(m_range),
                                std::ranges::range_size_t<range_t>(m_count));
    }


public:
    constexpr auto begin() noexcept
    {
        if constexpr (std::ranges::sized_range<range_t>)
        {
            if constexpr (std::ranges::random_access_range<range_t>)
            {
                return std::ranges::begin(m_range);
            }
            else {
                return std::counted_iterator{
                            std::ranges::begin(m_range),
                            std::ranges::range_difference_t<range_t>(size()) };
            }
        } else {
            return std::counted_iterator{ std::ranges::begin(m_range), m_count };
        }
    }

    constexpr auto begin() const noexcept requires(std::ranges::range<const range_t> )
    {
        if constexpr (std::ranges::sized_range<const range_t>)
        {
            if constexpr (std::ranges::random_access_range<const range_t>)
            {
                return std::make_const_iterator(std::ranges::begin(m_range));
            }
            else {
                return std::make_const_iterator(
                            std::counted_iterator{
                                std::ranges::begin(m_range),
                                std::ranges::range_difference_t<const range_t>(size()) });
            }
        }
        else {
            return std::make_const_iterator(
                        std::counted_iterator{ std::ranges::begin(m_range), m_count });
        }
    }

    constexpr auto end() noexcept
    {
        if constexpr (std::ranges::sized_range<range_t>)
        {
            if constexpr (std::ranges::random_access_range<range_t>)
            {
                return std::make_const_sentinel(
                            std::ranges::begin(m_range) + difference_type(size()));
            }
            else {
                return std::default_sentinel;
            }
        }
        else {
            return sentinel{ std::ranges::end(m_range) };
        }
    }

    constexpr auto end() const noexcept requires( std::ranges::range<const range_t> )
    {
        if constexpr (std::ranges::sized_range<const range_t>)
        {
            if constexpr (std::ranges::random_access_range<const range_t>)
            {
                return std::make_const_sentinel(
                            std::ranges::begin(m_range) +
                            std::ranges::range_difference_t<const range_t>(size()) );
            }
            else {
                return std::default_sentinel;
            }
        }
        else {
            return sentinel{ std::ranges::end(m_range) };
        }
    }

private:
    range_t         m_range;
    difference_type m_count;
};


template <std::ranges::view range_t>
struct take_view<range_t>::sentinel : public std::default_sentinel_t
{
public:
    using iterator = std::counted_iterator<std::ranges::iterator_t<range_t>>;

public:
    constexpr sentinel() noexcept = default;

    constexpr sentinel(std::ranges::sentinel_t<range_t> end) noexcept
        : m_end{std::move(end)}
    {}

public:
    [[nodiscard]]
    friend constexpr auto operator==(iterator const& lhs,
                                     sentinel const& rhs) noexcept -> bool
    {
        return (lhs.count() == 0 or lhs.base() == rhs.m_end);
    }

    [[nodiscard]]
    friend constexpr auto operator==(sentinel const& lhs,
                                     sentinel const& rhs) noexcept -> bool {
        return (rhs.m_end == lhs.m_end);
    }

private:
    std::ranges::sentinel_t<range_t> m_end{};
};


template <typename range_t>
take_view(range_t&&, std::ranges::range_difference_t<range_t>) -> take_view<std::views::all_t<range_t>>;


template <typename V>
inline constexpr auto std::ranges::enable_borrowed_range<take_view<V>> = std::ranges::enable_borrowed_range<V>;



template <std::integral Int>
struct take_view_closure
    : public std::ranges::range_adaptor_closure<take_view_closure<Int>>
{
public:
    constexpr take_view_closure(Int count) noexcept : m_count{count} {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& view) const noexcept
    {
        return take_view{std::forward<range_t>(view), m_count};
    }

private:
    Int m_count;
};


struct take_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
                typename difference_t = std::ranges::range_difference_t<range_t>>
    constexpr auto operator()(range_t&& view, difference_t count) const noexcept
    {
        return take_view{std::forward<range_t>(view), count};
    }

    constexpr auto operator()(std::integral auto count) const noexcept
    {
        return take_view_closure{count};
    }
};

namespace views {
    inline constexpr take_view_adaptor take{};
}

