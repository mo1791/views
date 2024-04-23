#include <functional>
#include <ranges>


// START UPPERCASE_VIEW
template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct uppercase_view : std::ranges::view_interface<uppercase_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;


public:
    constexpr uppercase_view() noexcept
        requires std::default_initializable<range_t>
    = default;

    constexpr uppercase_view(const range_t& range) noexcept
        requires std::copy_constructible<range_t>
        : m_range{ range }
    {}

    constexpr uppercase_view(range_t&& range) noexcept
        requires std::move_constructible<range_t>
        : m_range{std::move(range)}
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
    constexpr auto begin()       noexcept { return iterator{*this, std::ranges::begin(m_range) }; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return iterator{ *this, std::ranges::begin(m_range) };
    }
    
    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return sentinel{};
    }

private:
    range_t m_range = range_t{};
};


template <typename range_t>
uppercase_view(range_t&&) -> uppercase_view<std::views::all_t<range_t>>;





template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct uppercase_view<range_t>::sentinel: public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end( rhs->m_range ) );
    }
};



template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct uppercase_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(const uppercase_view& parent, iterator_t cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}

public:
    constexpr auto operator*() const noexcept
    {
        return std::isalpha(*m_cursor) ? m_and(*m_cursor, 0xdf) : *m_cursor;
    }

    constexpr auto operator*() noexcept
    {
        return std::isalpha(*m_cursor) ? m_and(*m_cursor, 0xdf) : *m_cursor;
    }


public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this); }

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
        m_cursor = std::ranges::prev(std::move(m_cursor));
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
                                     iterator const& rhs) noexcept -> bool
    {
        return (lhs.m_cursor == rhs.m_cursor);
    }

public:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<uppercase_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor      = iterator_t{} ;
    [[no_unique_address]] std::bit_and<char> m_and = std::bit_and{};
};
// END UPPERCAS_VIEW


// START LOWERCASE_VIEW
template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct lowercase_view : std::ranges::view_interface<lowercase_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;


public:
    constexpr lowercase_view() noexcept
        requires std::default_initializable<range_t>
    = default;

    constexpr lowercase_view(const range_t& range) noexcept
        requires std::copy_constructible<range_t>
        : m_range{ range }
    {}

    constexpr lowercase_view(range_t&& range) noexcept
        requires std::move_constructible<range_t>
        : m_range{std::move(range)}
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
    constexpr auto begin()       noexcept { return iterator{*this, std::ranges::begin(m_range) }; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return iterator{ *this, std::ranges::begin(m_range) };
    }
    
    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return sentinel{};
    }

private:
    range_t m_range = range_t{};
};


template <typename range_t>
lowercase_view(range_t&&) -> lowercase_view<std::views::all_t<range_t>>;





template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct lowercase_view<range_t>::sentinel: public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end( rhs->m_range ) );
    }
};



template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct lowercase_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(const lowercase_view& parent, iterator_t cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}

public:
    constexpr auto operator*() const noexcept
    {
        return std::isalpha(*m_cursor) ? m_or(*m_cursor, 0x20) : *m_cursor;
    }

    constexpr auto operator*() noexcept
    {
        return std::isalpha(*m_cursor) ? m_or(*m_cursor, 0x20) : *m_cursor;
    }


public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this); }

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
        m_cursor = std::ranges::prev(std::move(m_cursor));
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
                                     iterator const& rhs) noexcept -> bool
    {
        return (lhs.m_cursor == rhs.m_cursor);
    }

public:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<lowercase_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor    = iterator_t{} ;
    [[no_unique_address]] std::bit_or<char> m_or = std::bit_or{};
};
// END LOWERCASE_VIEW


// START TOGGLECASE_VIEW
template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct togglecase_view : std::ranges::view_interface<togglecase_view<range_t>>
{
public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;


public:
    constexpr togglecase_view() noexcept
        requires std::default_initializable<range_t>
    = default;

    constexpr togglecase_view(const range_t& range) noexcept
        requires std::copy_constructible<range_t>
        : m_range{ range }
    {}

    constexpr togglecase_view(range_t&& range) noexcept
        requires std::move_constructible<range_t>
        : m_range{std::move(range)}
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
    constexpr auto begin()       noexcept { return iterator{*this, std::ranges::begin(m_range) }; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return iterator{ *this, std::ranges::begin(m_range) };
    }
    
    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return sentinel{};
    }

private:
    range_t m_range = range_t{};
};


template <typename range_t>
togglecase_view(range_t&&) -> togglecase_view<std::views::all_t<range_t>>;





template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct togglecase_view<range_t>::sentinel: public std::default_sentinel_t
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
        return ( rhs.m_cursor == std::ranges::end( rhs->m_range ) );
    }
};



template <typename range_t>
    requires std::conjunction<
        std::bool_constant<std::ranges::input_range<range_t>>,
        std::bool_constant<
            std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
struct togglecase_view<range_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(const togglecase_view& parent, iterator_t cursor) noexcept
        : m_parent{ std::addressof(parent) }
        , m_cursor{ std::move(cursor) }
    {}

public:
    constexpr auto operator*() const noexcept
    {
        return std::isalpha(*m_cursor) ? m_xor(*m_cursor, 0x20) : *m_cursor;
    }

    constexpr auto operator*() noexcept
    {
        return std::isalpha(*m_cursor) ? m_xor(*m_cursor, 0x20) : *m_cursor;
    }


public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_cursor = std::ranges::next(std::move(m_cursor));
        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this); }

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
        m_cursor = std::ranges::prev(std::move(m_cursor));
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
                                     iterator const& rhs) noexcept -> bool
    {
        return (lhs.m_cursor == rhs.m_cursor);
    }

public:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<togglecase_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor    = iterator_t{} ;
    [[no_unique_address]] std::bit_xor<char> m_xor = std::bit_xor{};
};
// END TOGGLECASE_VIEW

struct uppercase_view_closure
    : public std::ranges::range_adaptor_closure<uppercase_view_closure>
{
public:
    template <typename range_t>
        requires std::conjunction<
            std::bool_constant<std::ranges::input_range<range_t>>,
            std::bool_constant<
                std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return uppercase_view{std::forward<range_t>(range)};
    }
};


struct lowercase_view_closure
    : public std::ranges::range_adaptor_closure<lowercase_view_closure>
{
public:
    template <typename range_t>
        requires std::conjunction<
            std::bool_constant<std::ranges::input_range<range_t>>,
            std::bool_constant<
                std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return lowercase_view{std::forward<range_t>(range)};
    }
};


struct togglecase_view_closure
    : public std::ranges::range_adaptor_closure<togglecase_view_closure>
{
   public:
    template <typename range_t>
        requires std::conjunction<
            std::bool_constant<std::ranges::input_range<range_t>>,
            std::bool_constant<
                std::same_as<char, std::ranges::range_value_t<range_t>>>>::value
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return togglecase_view{std::forward<range_t>(range)};
    }
};

namespace views {
    inline constexpr uppercase_view_closure  uppercase{};
    inline constexpr lowercase_view_closure  lowercase{};
    inline constexpr togglecase_view_closure togglecase{};
}  // namespace views

