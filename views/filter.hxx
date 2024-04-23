#include <functional>
#include <ranges>


template <
    std::ranges::view range_t,
    std::indirect_unary_predicate<std::ranges::iterator_t<range_t>> Predicate_t>
struct filter_view
    : public std::ranges::view_interface<filter_view<range_t, Predicate_t>>
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
    constexpr filter_view() noexcept
        requires(std::conjunction<
                    std::bool_constant<std::default_initializable<range_t>>,
                    std::bool_constant<std::default_initializable<Predicate_t>>,
                    std::bool_constant<
                        std::default_initializable<iterator_t>>>::value)
    = default;

    constexpr filter_view(range_t view, Predicate_t closure) noexcept
        requires(std::conjunction<
                    std::bool_constant<std::move_constructible<Predicate_t>>,
                    std::bool_constant<
                        std::default_initializable<iterator_t>>>::value)
        : m_view{std::move(view)}
        , m_closure{std::move(closure)}
    {
        m_cursor = std::ranges::begin(m_view);
        
        do {
            m_cursor = std::ranges::next(std::move(m_cursor));
        } while (m_cursor != std::ranges::end(m_view) &&
                 std::invoke(std::not_fn(m_closure), *m_cursor));
    }


public:
    constexpr auto begin()       noexcept { return iterator{*this, m_cursor}; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_iterator{*this, m_cursor};
    }

    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_sentinel{};
    }


private:
    range_t     m_view    = range_t{};
    Predicate_t m_closure = Predicate_t{};
    iterator_t  m_cursor  = iterator_t{};
};


template <typename View, typename Closure>
filter_view(View&&, Closure) -> filter_view<std::views::all_t<View>, Closure>;



template <
    std::ranges::view range_t,
    std::indirect_unary_predicate<std::ranges::iterator_t<range_t>> Predicate_t>
struct filter_view<range_t, Predicate_t>::sentinel : public std::default_sentinel_t
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
        return (rhs.m_cursor == std::ranges::end(rhs->m_view));
    }
};

template <
    std::ranges::view range_t,
    std::indirect_unary_predicate<std::ranges::iterator_t<range_t>> Predicate_t>
struct filter_view<range_t, Predicate_t>::const_sentinel : public std::default_sentinel_t
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
        return (rhs.m_cursor == std::ranges::end(rhs->m_view));
    }
};


template <
    std::ranges::view range_t,
    std::indirect_unary_predicate<std::ranges::iterator_t<range_t>> Predicate_t>
struct filter_view<range_t, Predicate_t>::iterator
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

    constexpr iterator(filter_view& parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {}

public:
    constexpr auto operator*() const noexcept { return *m_cursor; }
    constexpr auto operator*()       noexcept { return *m_cursor; }

public:
    constexpr auto operator++() noexcept -> iterator&
    {
        do {
            m_cursor = std::ranges::next(std::move(m_cursor));
        } while (m_cursor != std::ranges::end(m_parent->m_view) &&
                 std::invoke(std::not_fn(m_parent->m_closure), *m_cursor));

        return *this;
    }

    constexpr void operator++(int) noexcept { ++*this; }

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
        do {
            m_cursor = std::ranges::prev(std::move(m_cursor));
        } while (std::invoke(std::not_fn(m_parent->m_closure), *m_cursor));

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

public:
    [[nodiscard]]
    friend constexpr auto iter_move(const iterator& current) noexcept(
        noexcept(std::ranges::iter_move(current.m_cursor)))
    {
        return std::ranges::iter_move(current.m_cursor);
    }

    friend constexpr void
    iter_swap(const iterator& lhs, const iterator& rhs) noexcept(
        noexcept(std::ranges::iter_swap(lhs.m_cursor, rhs.m_cursor)))
        requires std::indirectly_swappable<iterator_t>
    {
        std::ranges::iter_swap(lhs.m_cursor, rhs.m_cursor);
    }


private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }


private:
    std::add_pointer_t<filter_view>  m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_cursor = iterator_t{};
};


template <
    std::ranges::view range_t,
    std::indirect_unary_predicate<std::ranges::iterator_t<range_t>> Predicate_t>
struct filter_view<range_t, Predicate_t>::const_iterator
{
public:
    friend struct const_sentinel;


public:
    using iterator_t = std::ranges::iterator_t<const range_t>;
    using sentinel_t = std::ranges::sentinel_t<const range_t>;

    using value_type = std::ranges::range_value_t<const range_t>;
    using reference  = std::ranges::range_reference_t<const range_t>;

    using difference_type = std::ranges::range_difference_t<const range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr const_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr const_iterator(const filter_view& parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {}

public:
    constexpr auto operator*() const noexcept { return *m_cursor; }
    constexpr auto operator*()       noexcept { return *m_cursor; }


public:
    constexpr auto operator++() noexcept -> const_iterator&
    {
        do {
            m_cursor = std::ranges::next(std::move(m_cursor));
        } while (m_cursor != std::ranges::end(m_parent->m_view) &&
                 std::invoke(std::not_fn(m_parent->m_closure), *m_cursor));

        return *this;
    }

    constexpr void operator++(int) noexcept { ++*this; }

    constexpr auto operator++(int) noexcept -> const_iterator
        requires std::ranges::forward_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance(*this, 1);

        return copy;
    }

    constexpr auto operator--() noexcept -> const_iterator&
        requires std::ranges::bidirectional_range<range_t>
    {
        do {
            m_cursor = std::ranges::prev(std::move(m_cursor));
        } while (std::invoke(std::not_fn(m_parent->m_closure), *m_cursor));

        return *this;
    }

    constexpr auto operator--(int) noexcept -> const_iterator
        requires std::ranges::bidirectional_range<range_t>
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

public:
    [[nodiscard]]
    friend constexpr auto iter_move(const const_iterator& current) noexcept(
        noexcept(std::ranges::iter_move(current.m_cursor)))
    {
        return std::ranges::iter_move(current.m_cursor);
    }

    friend constexpr void
    iter_swap(const const_iterator& lhs, const const_iterator& rhs) noexcept(
        noexcept(std::ranges::iter_swap(lhs.m_cursor, rhs.m_cursor)))
        requires std::indirectly_swappable<iterator_t>
    {
        std::ranges::iter_swap(lhs.m_cursor, rhs.m_cursor);
    }

private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<filter_view>>  m_parent = nullptr;
    std::ranges::iterator_t<std::add_const_t<range_t>> m_cursor = iterator_t{};
};


template <typename Closure>
struct filter_view_closure
    : public std::ranges::range_adaptor_closure<filter_view_closure<Closure>>
{
public:
    constexpr filter_view_closure(Closure closure) noexcept
        requires std::move_constructible<Closure>
        : m_closure{std::move(closure)}
    {}

public:
    template <typename View>
    constexpr auto operator()(View&& view) const noexcept
    {
        return filter_view{std::forward<View>(view), std::move(m_closure)};
    }

private:
    Closure m_closure;
};


struct filter_view_adaptor
{
public:
    template <typename View, typename Closure>
    constexpr auto operator()(View&& view, Closure closure) const noexcept
    {
        return filter_view{std::forward<View>(view), std::move(closure)};
    }

    template <typename Closure>
    constexpr auto operator()(Closure closure) const noexcept
    {
        return filter_view_closure{std::move(closure)};
    }
};

namespace views {
    inline constexpr filter_view_adaptor filter{};
}


