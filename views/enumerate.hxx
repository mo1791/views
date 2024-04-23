#include <ranges>



template <std::ranges::view range_t>
struct enumerate_view
    : public std::ranges::view_interface<enumerate_view<range_t>>
{
public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;

public:
    constexpr enumerate_view() noexcept
        requires std::default_initializable<range_t> = default;

    constexpr enumerate_view(range_t view) noexcept
        : m_view{ std::move(view) }
    {}

public:
    constexpr auto operator[](difference_type index) const noexcept
        -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_view[index];
    }

    constexpr auto operator[](difference_type index) noexcept
        -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_view[index];
    }


public:
    [[nodiscard]]
    constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t>
    {
        return std::ranges::size(m_view);
    }

    [[nodiscard]]
    constexpr auto size() const noexcept -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t>
    {
        return std::ranges::size(m_view);
    }


public:
    constexpr auto begin()       noexcept { return iterator{ *this, std::ranges::begin(m_view) }; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return iterator{ *this, std::ranges::begin(m_view) };
    }
    
    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return sentinel{};
    }

private:
    range_t m_view = range_t{};
};


template <typename range_t>
enumerate_view(range_t&&) -> enumerate_view<std::views::all_t<range_t>>;


template <typename range_t>
inline constexpr auto std::ranges::enable_borrowed_range<enumerate_view<range_t>> =
    std::ranges::enable_borrowed_range<range_t>;



template <std::ranges::view range_t>
struct enumerate_view<range_t>::sentinel: public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const sentinel&,
                                     const sentinel&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const sentinel&,
                                     const iterator& rhs) noexcept -> bool {
        return ( rhs.m_cursor == std::ranges::end( rhs->m_view ) );
    }
};


template <std::ranges::view range_t>
struct enumerate_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::tuple<std::ranges::range_difference_t<range_t>,
                                    std::ranges::range_value_t<range_t>>;
    
    using reference  = std::tuple<std::ranges::range_difference_t<range_t>,
                                    std::ranges::range_reference_t<range_t>>;
    
    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>  = default;

    constexpr iterator(const enumerate_view& parent, iterator_t cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
        , m_index { 0UL }
    {}

public:
    constexpr auto operator*() const noexcept -> reference
    {
        return { m_index, *m_cursor };
    }

    constexpr auto operator*() noexcept -> reference
    {
        return { m_index, *m_cursor };
    }

public:
    constexpr auto operator[](difference_type index) const noexcept
        -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_cursor[index];
    }

    constexpr auto operator[](difference_type index) noexcept -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_cursor[index];
    }

public:
    constexpr auto operator++() noexcept -> iterator&
    {
        ++m_cursor;
        ++m_index;

        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance( *this, 1 ); }
    
    constexpr auto operator++(int) noexcept -> iterator
        requires std::ranges::forward_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, 1 );

        return copy;
    }

    constexpr auto operator--() noexcept -> iterator& 
        requires std::ranges::bidirectional_range<range_t>
    {
        --m_cursor;
        --m_index;

        return *this;
    }

    constexpr auto operator--(int) noexcept -> iterator
        requires std::ranges::bidirectional_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, -1 );

        return copy;
    }

    constexpr auto operator+=(difference_type step) noexcept -> iterator& 
        requires std::ranges::random_access_range<range_t>
    {
        m_cursor += step;
        m_index  += step;

        return *this;
    }

    constexpr auto operator-=(difference_type step) noexcept
        -> iterator&
        requires std::ranges::random_access_range<range_t>
    {
        m_cursor -= step;
        m_index  -= step;

        return *this;
    }


public: 
    [[nodiscard]]
    friend constexpr auto operator+(iterator lhs,
                                    difference_type step) noexcept -> iterator
    {
        return lhs += step;
    }

    [[nodiscard]]
    friend constexpr auto operator+(difference_type step,
                                    iterator rhs) noexcept -> iterator
    {
        return (rhs + step);
    }

    [[nodiscard]]
    friend constexpr auto operator-(iterator lhs,
                                    difference_type step) noexcept -> iterator
    {
        return lhs -= step;
    }

    [[nodiscard]]
    friend constexpr auto operator-(difference_type step,
                                    iterator rhs) noexcept -> iterator
    {
        return (rhs - step);
    }

public:
    [[nodiscard]]
    friend constexpr auto operator==(iterator const& lhs,
                                     iterator const& rhs) noexcept -> bool {
        return (lhs.m_cursor == rhs.m_cursor);
    }

public:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<enumerate_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t>         m_cursor = iterator_t{};
    std::ranges::range_difference_t<range_t> m_index  = 0UL;
};




class enumerate_view_closure
    : public std::ranges::range_adaptor_closure<enumerate_view_closure>
{
public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& view) const noexcept
    {
        return enumerate_view{ std::forward<range_t>(view) };
    }
};



namespace views {
    inline constexpr enumerate_view_closure enumerate{};
}
