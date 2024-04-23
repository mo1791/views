#include <functional>
#include <ranges>




template <std::ranges::view range_t>
struct chunk_view : public std::ranges::view_interface<chunk_view<range_t>>
{
public:
    struct outer_iterator;
    struct outer_sentinel;
    struct inner_iterator;
    struct inner_sentinel;


public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;
    using size_type  = std::ranges::range_size_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    constexpr chunk_view() noexcept
        requires std::default_initializable<range_t>
    = default;


    constexpr chunk_view(range_t range, difference_type count) noexcept
        requires std::default_initializable<iterator_t>
        : m_view{std::move(range)}
        , m_count{count}
    {
    }

   
public:
    constexpr auto begin() noexcept
    {
        return outer_iterator{*this, std::ranges::begin(m_view) };
    }

    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return outer_iterator{ *this, std::ranges::begin(m_view) };
    }

    constexpr auto end()       noexcept { return outer_sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return outer_sentinel{};
    }


private:
    range_t          m_view = range_t{};
    difference_type m_count = 0UL;
};

template <typename range_t>
chunk_view(range_t&&, std::ranges::range_difference_t<range_t>)
    -> chunk_view<std::views::all_t<range_t>>;



template <std::ranges::view range_t>
struct chunk_view<range_t>::outer_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(outer_sentinel const&,
                                     outer_sentinel const&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(outer_sentinel const&,
                                     outer_iterator const& rhs) noexcept -> bool {
        return rhs.m_cursor    == std::ranges::end(rhs->m_view) &&
               rhs.m_remainder != 0UL;
    }
};


template <std::ranges::view range_t>
struct chunk_view<range_t>::outer_iterator
{
public:
    friend struct outer_sentinel;

public:
    struct inner_view;

public:
    using range_iterator_t = std::ranges::iterator_t<range_t>;
    using range_value_type = std::ranges::range_value_t<range_t>;
    using range_reference  = std::ranges::range_reference_t<range_t>;

    using value_type = inner_view;
    using reference  = inner_view;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr outer_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr outer_iterator(const chunk_view& parent, iterator_t cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {
        m_remainder = m_parent->m_count;
    }


public:
    constexpr auto operator*() const noexcept -> value_type
    {
        return inner_view{ m_parent, m_cursor, m_remainder };
    }

    constexpr auto operator*() noexcept -> value_type
    {
        return inner_view{ m_parent, m_cursor, m_remainder };
    }

public:
    constexpr auto operator++() noexcept -> outer_iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor), m_remainder,
                                     std::ranges::end(m_parent->m_view));

        m_remainder = m_parent->m_count;

        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance( *this, 1 ); }

public:
    [[nodiscard]]
    friend constexpr auto operator==(outer_iterator const& lhs,
                                     outer_iterator const& rhs) noexcept -> bool {
        return lhs.m_cursor == rhs.m_cursor;
    }

private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<chunk_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t>         m_cursor    = iterator_t{};
    std::ranges::range_difference_t<range_t> m_remainder = 0UL;
};


template <std::ranges::view range_t>
struct chunk_view<range_t>::outer_iterator::inner_view
    : public std::ranges::view_interface<inner_view>
{
public:
    public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    constexpr inner_view() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr inner_view(const chunk_view* parent, iterator_t cursor,
                                                   difference_type remainder) noexcept
        : m_parent{ parent }
        , m_cursor{ std::move(cursor) }
    {
        m_remainder = remainder;
    }

public:
    constexpr auto begin()       noexcept { return inner_iterator{*m_parent, m_cursor, m_remainder}; }
    constexpr auto begin() const noexcept { return inner_iterator{*m_parent, m_cursor, m_remainder}; }
    
    constexpr auto end()       noexcept { return inner_sentinel{}; }
    constexpr auto end() const noexcept { return inner_sentinel{}; }


private:
    std::add_pointer_t<std::add_const_t<chunk_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t>         m_cursor    = iterator_t{};
    std::ranges::range_difference_t<range_t> m_remainder = 0UL;
};



template <std::ranges::view range_t>
struct chunk_view<range_t>::inner_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(inner_sentinel const&,
                                     inner_sentinel const&)noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(inner_sentinel const&,
                                     inner_iterator const& rhs) noexcept -> bool {
        return rhs.m_remainder == 0UL;
    }
};

template <std::ranges::view range_t>
struct chunk_view<range_t>::inner_iterator
{
public:
    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr inner_iterator() noexcept : m_parent{ nullptr } {}

    constexpr inner_iterator(const chunk_view& parent, iterator_t cursor,
                                                       difference_type remainder) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {
        m_remainder = remainder;
    }


public:
    constexpr auto operator*()       noexcept -> reference { return *m_cursor; }
    constexpr auto operator*() const noexcept -> reference { return *m_cursor; }


public:
    constexpr auto operator++() noexcept -> inner_iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));

        if (m_cursor == std::ranges::end(m_parent->m_view))
            m_remainder = 0UL;
        else
            m_remainder = m_remainder - 1;

        return *this;
    }

    constexpr void operator++(int) noexcept{ std::ranges::advance( *this, 1 ); }


public:
    [[nodiscard]]
    friend constexpr auto operator==(inner_iterator const& lhs,
                                     inner_iterator const& rhs) noexcept -> bool {
        return ( lhs.m_cursor == rhs.m_cursor );
    }

public:
    [[nodiscard]]
    friend constexpr auto operator-(inner_sentinel const&,
                                    inner_iterator const& rhs) noexcept -> difference_type
        requires std::sized_sentinel_for<std::ranges::sentinel_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
    {
        return std::ranges::min( rhs.m_remainder,
                                ( std::ranges::end(rhs->m_view) - rhs.m_cursor ) );
    }

public:
    std::add_pointer_t<std::add_const_t<chunk_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t>         m_cursor    = iterator_t{};
    std::ranges::range_difference_t<range_t> m_remainder = 0UL;
};



template <std::integral Int>
struct chunk_view_closure
    : public std::ranges::range_adaptor_closure<chunk_view_closure<Int>>
{
public:
    constexpr chunk_view_closure(Int count) noexcept : m_count{count} {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return chunk_view{ std::forward<range_t>(range), m_count };
    }

public:
    Int m_count;
};

struct chunk_view_adaptor
{
public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(
        range_t&& range,
        std::ranges::range_difference_t<range_t> count) const noexcept
    {
        return chunk_view{ std::forward<range_t>(range), count };
    }

    constexpr auto operator()(std::integral auto count) const noexcept
    {
        return chunk_view_closure{ count };
    }
};

namespace views {
    inline constexpr chunk_view_adaptor chunk{};
}

