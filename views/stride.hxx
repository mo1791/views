#include <ranges>



template <std::ranges::view range_t>
struct stride_view : public std::ranges::view_interface<stride_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;

   
public:
    friend struct iterator;
    friend struct sentinel;

   
public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;


public:
    constexpr stride_view() noexcept
        requires std::default_initializable<range_t>
    = default;


    constexpr stride_view(range_t view, difference_type step) noexcept
        : m_view{std::move(view)}
        , m_step{step}
    {
    }


public:
    [[nodiscard]]
    constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t>
    {
        return ( ( std::ranges::size(m_view) / m_step ) + 
                    ( std::ranges::size(m_view) % m_step ? 1 : 0 ) );
    }

    [[nodiscard]]
    constexpr auto size() const noexcept -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t>
    {
        return ( ( std::ranges::size(m_view) / m_step ) + 
                    ( std::ranges::size(m_view) % m_step ? 1 : 0 ) );
    }

public:
    constexpr auto begin()       noexcept { return iterator{*this, std::ranges::begin(m_view)}; }
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
    range_t          m_view = range_t{};
    difference_type  m_step = 0UL;
};


template <typename range_t>
stride_view(range_t&&, std::ranges::range_difference_t<range_t>)
    -> stride_view<std::views::all_t<range_t>>;



template <std::ranges::view range_t>
struct stride_view<range_t>::sentinel : public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end( rhs->m_view ) );
    }
};



template <std::ranges::view range_t>
struct stride_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;
   
    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::forward_iterator_tag;
    using iterator_concept  = std::forward_iterator_tag;

public:
    constexpr iterator() noexcept : m_parent{ nullptr } {}

    constexpr iterator(const stride_view& parent,
                        std::ranges::iterator_t<range_t> cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}

public:
    constexpr auto operator*() const noexcept -> reference { return *m_cursor; }
    constexpr auto operator*()       noexcept -> reference { return *m_cursor; }

public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor), m_parent->m_step,
                                     std::ranges::end(m_parent->m_view));
        return *this;
    }

    constexpr void operator++(int) noexcept
    {
        std::ranges::advance( *this, 1 );
    }

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
        m_cursor = std::ranges::prev(std::move(m_cursor), m_parent->m_step,
                                     std::ranges::begin(m_parent->m_view));
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
        return (lhs->m_cursor == rhs->m_cursor);
    }

private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }

private:
    std::add_pointer_t<std::add_const_t<stride_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor = iterator_t{};
};





template <std::integral Int>
struct stride_view_closure
    : public std::ranges::range_adaptor_closure<stride_view_closure<Int>>
{
public:
    constexpr stride_view_closure(Int step) noexcept : m_step{ step } {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& view) const noexcept
    {
        return stride_view{ std::forward<range_t>(view), m_step };
    }

private:
    Int m_step;
};


struct stride_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
                typename difference_t = std::ranges::range_difference_t<range_t>>
    constexpr auto operator()(range_t&& view, difference_t step) const noexcept
    {
        return stride_view{ std::forward<range_t>(view), step };
    }

    constexpr auto operator()(std::integral auto step) const noexcept
    {
        return stride_view_closure{ step };
    }
};

namespace views {
    inline constexpr stride_view_adaptor stride{};
}