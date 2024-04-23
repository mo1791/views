#include <ranges>



template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::viewable_range<range_t>>,
        std::bool_constant<std::ranges::input_range<range_t>>
    >::value
struct addressof_view
    : public std::ranges::view_interface<addressof_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;
    struct const_iterator;
    struct const_sentinel;


public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::add_pointer_t<std::ranges::range_value_t<range_t>>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr addressof_view() noexcept
        requires std::default_initializable<range_t> = default;

    constexpr addressof_view(range_t range) noexcept
        requires std::move_constructible<range_t>
        : m_range{std::move(range)}
    {}

public:
    [[nodiscard]] constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t> {
        return std::ranges::size( m_range );
    }

    [[nodiscard]] constexpr auto size() const noexcept
        -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t> {
        return std::ranges::size( m_range);
    }

public:
    constexpr auto begin()       noexcept { return iterator{ *this, std::ranges::begin(m_range) }; }
    constexpr auto begin() const noexcept
        requires( std::ranges::range<const range_t> )
    {
        return const_iterator{ *this, std::ranges::begin(m_range) };
    }

    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires( std::ranges::range<const range_t> )
    {
        return const_sentinel{};
    }

private:
    range_t m_range = range_t{};
};


template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::viewable_range<range_t>>,
        std::bool_constant<std::ranges::input_range<range_t>>
    >::value
struct addressof_view<range_t>::sentinel: public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end(rhs->m_range) );
    }
};


template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::viewable_range<range_t>>,
        std::bool_constant<std::ranges::input_range<range_t>>
    >::value
struct addressof_view<range_t>::const_sentinel: public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end(rhs->m_range) );
    }
};


template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::viewable_range<range_t>>,
        std::bool_constant<std::ranges::input_range<range_t>>
    >::value
struct addressof_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::add_pointer_t<std::ranges::range_value_t<range_t>>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::bidirectional_iterator_tag;
    using iterator_concept  = std::bidirectional_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t> = default;

    constexpr iterator(addressof_view& parent, iterator_t cursor) noexcept
        requires std::move_constructible<iterator_t>
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}


public:
    constexpr auto operator*() const noexcept { return std::addressof(*m_cursor); }
    constexpr auto operator*()       noexcept { return std::addressof(*m_cursor); }


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
        std::ranges::advance( *this, 1 );

        return copy;
    }

    constexpr auto operator--() noexcept -> iterator &
        requires std::ranges::bidirectional_range<range_t>
    {
        m_cursor = std::ranges::prev( std::move(m_cursor) );
        return *this;
    }

    constexpr auto operator--(int) noexcept -> iterator
        requires std::ranges::bidirectional_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, -1 );

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
    std::add_pointer_t<addressof_view> m_parent = nullptr;
    std::ranges::iterator_t<range_t>   m_cursor = iterator_t{};
};


template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::viewable_range<range_t>>,
        std::bool_constant<std::ranges::input_range<range_t>>
    >::value
struct addressof_view<range_t>::const_iterator
{
public:
    friend struct const_sentinel;

public:
    using iterator_t = std::ranges::iterator_t<const range_t>;
    using sentinel_t = std::ranges::sentinel_t<const range_t>;

    using value_type = std::add_pointer_t<std::ranges::range_value_t<const range_t>>;
    using reference  = std::ranges::range_reference_t<const range_t>;

    using difference_type = std::ranges::range_difference_t<const range_t>;

    using iterator_category = std::bidirectional_iterator_tag;
    using iterator_concept  = std::bidirectional_iterator_tag;


public:
    constexpr const_iterator() noexcept
        requires std::default_initializable<iterator_t> = default;

    constexpr const_iterator(const addressof_view& parent,iterator_t cursor) noexcept
        requires std::move_constructible<iterator_t>
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}


public:
    constexpr auto operator*() const noexcept { return std::addressof(*m_cursor); }
    constexpr auto operator*()       noexcept { return std::addressof(*m_cursor); }


public:
    constexpr auto operator++() noexcept -> const_iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance( *this, 1); }

    constexpr auto operator++(int) noexcept -> const_iterator
        requires std::ranges::forward_range<const range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, 1 );

        return copy;
    }

    constexpr auto operator--() noexcept -> const_iterator &
        requires std::ranges::bidirectional_range<const range_t>
    {
        m_cursor = std::ranges::prev( std::move(m_cursor) );
        return *this;
    }

    constexpr auto operator--(int) noexcept -> const_iterator
        requires std::ranges::bidirectional_range<const range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, -1 );

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
    std::add_pointer_t<std::add_const_t<addressof_view>> m_parent = nullptr;
    std::ranges::iterator_t<std::add_const_t<range_t>>   m_cursor = iterator{};
};


template <typename range_t>
addressof_view(range_t&&) -> addressof_view<std::views::all_t<range_t>>;



struct addressof_view_closure
    : std::ranges::range_adaptor_closure<addressof_view_closure>
{
public:
    template <typename range_t>
        requires std::conjunction<
            std::bool_constant<std::ranges::viewable_range<range_t>>,
            std::bool_constant<std::ranges::input_range<range_t>>
        >::value
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return addressof_view{std::forward<range_t>(range)};
    }
};

namespace views {
    inline constexpr ::addressof_view_closure addressof{};
}


