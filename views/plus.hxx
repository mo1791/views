#include <ranges>
#include <functional>



namespace details {
    template <typename LHS, typename RHS>
    concept Addable = requires(LHS lhs, RHS rhs) { lhs + rhs; };
}  // namespace details


// Custom view adaptor that adds a specified value to each element of a range.
template <std::ranges::view range_t>
struct divides_view : public std::ranges::view_interface<divides_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;
    struct const_iterator;
    struct const_sentinel;

public:
    friend struct iterator;
    friend struct sentinel;
    friend struct const_iterator;
    friend struct const_sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

public:
    constexpr divides_view() noexcept
        requires(std::conjunction<
                    std::bool_constant<std::default_initializable<range_t>>,
                    std::bool_constant<std::default_initializable<value_type>>
                >::value)
    = default;

    constexpr divides_view(range_t const& range, value_type const& value) noexcept
        : m_range{ range }
        , m_value{ value }
    {}

    constexpr divides_view(range_t&& range, value_type&& value) noexcept
        : m_range{std::move(range)}
        , m_value{std::move(value)}
    {}

public:
    [[nodiscard]]
    constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t>
    {
        return std::ranges::size(m_range);
    }

    [[nodiscard]]
    constexpr auto size() const noexcept -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t>
    {
        return std::ranges::size(m_range);
    }

public:
    constexpr auto begin() noexcept
    {
        return iterator{ *this, std::ranges::begin(m_range) };
    }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_iterator{ *this, std::ranges::begin(m_range) };
    }

    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_sentinel{};
    }

private:
    range_t    m_range = range_t{};    // Underlying range
    value_type m_value = value_type{}; // Value to be added
};


template <typename Range>
divides_view(Range&&, std::ranges::range_value_t<Range>) -> divides_view<std::views::all_t<Range>>;



template <std::ranges::view Range>
struct divides_view<Range>::sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(sentinel const&,
                                     sentinel const&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(sentinel const&,
                                     iterator const& rhs) noexcept -> bool {
        return (rhs.m_cursor == std::ranges::end(rhs.m_range));
    }
    /****
     * As of C++20, the compiler uses this^ to generate these:
     * sentinel != iterator
     * iterator == sentinel
     * iterator != sentinel
     *****/
};

template <std::ranges::view Range>
struct divides_view<Range>::const_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const_sentinel const&,
                                     const_sentinel const&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const_sentinel const&,
                                     const_iterator const& rhs) noexcept -> bool {
        return (rhs.m_cursor == std::ranges::end(rhs->m_range));
    }
    /****
     * As of C++20, the compiler uses this^ to generate these:
     * const_sentinel != const_iterator
     * const_iterator == const_sentinel
     * const_iterator != const_sentinel
     *****/
};


template <std::ranges::view range_t>
struct divides_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;
    using pointer    = void;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(divides_view& parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {}

public:
    constexpr auto operator*() const noexcept
    {
        return std::invoke(std::plus{}, *m_cursor, m_parent->m_value);
    }

    constexpr auto operator*() noexcept
    {
        return std::invoke(std::plus{}, *m_cursor, m_parent->m_value);
    }

public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this, 1); }

    constexpr auto operator++(int) noexcept -> iterator
        requires std::ranges::forward_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance(*this, 1);

        return copy;
    }

    constexpr auto operator--() noexcept -> iterator&
        requires std::ranges::bidirectional_range<range_t>
    {
        m_cursor = std::ranges::prev(std::move(m_cursor));
        return *this;
    }

    constexpr auto operator--(int) noexcept -> iterator
        requires std::ranges::bidirectional_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance(*this, -1);

        return copy;
    }

public:
    [[nodiscard]]
    friend constexpr auto operator==(iterator const& lhs,
                                     iterator const& rhs) noexcept -> bool {
        return (lhs.m_cursor == rhs.m_cursor);
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }

private:
    std::add_pointer_t<divides_view> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor = iterator_t{};
};


template <std::ranges::view range_t>
struct divides_view<range_t>::const_iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<const range_t>;
    using value_type = std::ranges::range_value_t<const range_t>;
    using reference  = std::ranges::range_reference_t<const range_t>;
    using pointer    = void;

    using difference_type = std::ranges::range_difference_t<const range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr const_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr const_iterator(const divides_view& parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {}

public:
    constexpr auto operator*() const noexcept
    {
        return std::invoke(std::plus{}, *m_cursor, m_parent->m_value);
    }

    constexpr auto operator*() noexcept
    {
        return std::invoke(std::plus{}, *m_cursor, m_parent->m_value);
    }

public:
    constexpr auto operator++() noexcept -> const_iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this, 1); }

    constexpr auto operator++(int) noexcept -> const_iterator
        requires std::ranges::forward_range<const range_t>
    {
        auto copy = *this;
        std::ranges::advance(*this, 1);

        return copy;
    }

    constexpr auto operator--() noexcept -> const_iterator&
        requires std::ranges::bidirectional_range<const range_t>
    {
        m_cursor = std::ranges::prev(std::move(m_cursor));
        return *this;
    }

    constexpr auto operator--(int) noexcept -> const_iterator
        requires std::ranges::bidirectional_range<const range_t>
    {
        auto copy = *this;
        std::ranges::advance(*this, -1);

        return copy;
    }

public:
    [[nodiscard]]
    friend constexpr auto operator==(const_iterator const& lhs,
                                     const_iterator const& rhs) noexcept -> bool {
        return (lhs.m_cursor == rhs.m_cursor);
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }

private:
    std::add_pointer_t<std::add_const_t<divides_view>> m_parent = nullptr;
    std::ranges::iterator_t<std::add_const_t<range_t>> m_cursor = iterator_t{};
};


// Closure adaptor for divides_view
template <typename T>
struct divides_view_closure
    : public std::ranges::range_adaptor_closure<divides_view_closure<T>>
{
public:
    constexpr divides_view_closure(T count) noexcept
        requires std::move_constructible<T>
        : m_value{std::move(count)}
    {}

public:
    template <typename range_t>
        requires std::conjunction<
                    std::bool_constant<std::ranges::input_range<range_t>>,
                    std::bool_constant<
                        details::Addable< T, std::ranges::range_value_t<range_t>>>
                >::value
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return divides_view{std::forward<range_t>(range), std::move(m_value)};
    }

private:
    T m_value;
};


struct divides_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
              typename value_t = std::ranges::range_value_t<range_t>>
    constexpr auto operator()(range_t &&range, value_t value) const noexcept
    {
        return divides_view{std::forward<range_t>(range), std::move(value)};
    }

    template <typename T>
    constexpr auto operator()(T count) const noexcept
    {
        return divides_view_closure<T>{std::move(count)};
    }
};

namespace views {
    inline constexpr divides_view_adaptor plus{};
}


