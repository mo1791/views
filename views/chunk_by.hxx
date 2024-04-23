#include <functional>
#include <ranges>



template <std::ranges::input_range range_t,
          std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
              Predicate_t>
struct chunk_by_view
    : public std::ranges::view_interface<chunk_by_view<range_t, Predicate_t>>
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
    using size_type  = std::ranges::range_size_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

public:
    constexpr chunk_by_view() noexcept
        requires( std::conjunction<
                     std::bool_constant<std::default_initializable<range_t>>,
                     std::bool_constant<
                         std::default_initializable<Predicate_t>>,
                     std::bool_constant<
                         std::default_initializable<iterator_t>>>::value )
    = default;

    constexpr chunk_by_view(range_t range, Predicate_t closure) noexcept
        requires( std::conjunction<
                     std::bool_constant<std::move_constructible<range_t>>,
                     std::bool_constant<std::move_constructible<Predicate_t>>,
                     std::bool_constant<
                         std::default_initializable<iterator_t>>>::value )
        : m_view{std::move(range)}
        , m_closure{std::move(closure)}
    {
        std::ranges::iterator_t<range_t> current{};

        if (std::ranges::empty(m_view))
        {
            current = std::ranges::end(m_view);
            m_next = std::ranges::end(m_view);
        }
        else {
            current = std::ranges::begin(m_view);

            m_next = std::ranges::next(
                std::ranges::adjacent_find(current, std::ranges::end(m_view), std::not_fn(m_closure)),
                1, std::ranges::end(m_view));
        }
    }

public:
    constexpr auto begin() noexcept
    {
        return iterator{*this, std::ranges::begin(m_view), m_next};
    }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_iterator{ *this, std::ranges::begin(m_view), m_next };
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
    iterator_t  m_next    = iterator_t{};
};


template <typename range_t, typename Closure>
chunk_by_view(range_t&&, Closure) -> chunk_by_view<std::views::all_t<range_t>, Closure>;



template <std::ranges::input_range range_t,
          std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
              Predicate_t>
struct chunk_by_view<range_t, Predicate_t>::sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(sentinel const&,
                                     sentinel const&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const sentinel&,
                                     const iterator& rhs) noexcept -> bool {
        return (rhs.m_current == std::ranges::end(rhs->m_view));
    }
};


template <std::ranges::input_range range_t,
          std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
              Predicate_t>
struct chunk_by_view<range_t, Predicate_t>::const_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const_sentinel const&,
                                     const_sentinel const&) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const const_sentinel&,
                                     const const_iterator& rhs) noexcept -> bool {
        return (rhs.m_current == std::ranges::end(rhs->m_view));
    }
};


template <std::ranges::input_range range_t,
          std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
              Predicate_t>
struct chunk_by_view<range_t, Predicate_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::subrange<iterator_t>;
    using reference  = std::ranges::subrange<iterator_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(chunk_by_view& parent, iterator_t current,
                                              iterator_t next) noexcept
        : m_parent{std::addressof(parent)}
        , m_current{std::move(current)}
        , m_next{std::move(next)}
    {}

public:
    constexpr auto operator*() const noexcept -> value_type
    {
        return { m_current, m_next };
    }

    constexpr auto operator*() noexcept -> value_type
    {
        return { m_current, m_next };
    }


public:
    constexpr auto operator++() noexcept -> iterator&
    {
        m_current = std::move(m_next);
        m_next    = std::ranges::next(
                        std::ranges::adjacent_find(
                                m_current, std::ranges::end(m_parent->m_view),
                                std::not_fn(m_parent->m_closure)
                            ),
                    1, std::ranges::end(m_parent->m_view));

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


public:
    [[nodiscard]]
    friend constexpr auto operator==(const iterator& lhs,
                                     const iterator& rhs) noexcept -> bool {
        return (lhs.m_current == rhs.m_current);
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }

private:
    std::add_pointer_t<chunk_by_view> m_parent  = nullptr;
    std::ranges::iterator_t<range_t>  m_current = iterator_t{};
    std::ranges::iterator_t<range_t>  m_next    = iterator_t{};
};


template <std::ranges::input_range range_t,
          std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                         std::ranges::iterator_t<range_t>>
              Predicate_t>
struct chunk_by_view<range_t, Predicate_t>::const_iterator
{
public:
    friend struct const_sentinel;

public:
    using iterator_t = std::ranges::iterator_t<const range_t>;
    using sentinel_t = std::ranges::sentinel_t<const range_t>;

    using value_type = std::ranges::subrange<iterator_t>;
    using reference  = std::ranges::subrange<iterator_t>;

    using difference_type = std::ranges::range_difference_t<const range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr const_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr const_iterator(const chunk_by_view& parent, iterator_t current,
                                                          iterator_t next) noexcept
        : m_parent{std::addressof(parent)}
        , m_current{std::move(current)}
        , m_next{std::move(next)}
    {}

public:
    constexpr auto operator*() const noexcept -> value_type
    {
        return {m_current, m_next};
    }

    constexpr auto operator*() noexcept -> value_type
    {
        return {m_current, m_next};
    }

public:
    constexpr auto operator++() noexcept -> const_iterator&
    {
        m_current = std::move(m_next);
        m_next    = std::ranges::next(
                        std::ranges::adjacent_find(
                            m_current, std::ranges::end(m_parent->m_view),
                            std::not_fn(m_parent->m_closure)
                        ),
                    1, std::ranges::end(m_parent->m_view));

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


public:
    [[nodiscard]]
    friend constexpr auto operator==(const const_iterator& lhs,
                                     const const_iterator& rhs) noexcept -> bool {
        return (lhs.m_current == rhs.m_current);
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<chunk_by_view>> m_parent = nullptr;
    std::ranges::iterator_t<const range_t> m_current = iterator_t{};
    std::ranges::iterator_t<const range_t> m_next    = iterator_t{};
};



template <typename Closure>
struct chunk_by_view_closure : public std::ranges::range_adaptor_closure<
                                   chunk_by_view_closure<Closure>>
{
public:
    constexpr chunk_by_view_closure(Closure closure) noexcept
        : m_closure{std::move(closure)}
    {}

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t&& range) const noexcept
    {
        return chunk_by_view{std::forward<range_t>(range), std::move(m_closure)};
    }

private:
    Closure m_closure;
};


struct chunk_by_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
              std::indirect_binary_predicate<std::ranges::iterator_t<range_t>,
                                             std::ranges::iterator_t<range_t>>
                  Predicate_t>
    constexpr auto operator()(range_t&& range,
                              Predicate_t closure) const noexcept
    {
        return chunk_by_view{std::forward<range_t>(range), std::move(closure)};
    }

    template <typename Predicate_t>
    constexpr auto operator()(Predicate_t closure) const noexcept
    {
        return chunk_by_view_closure{std::move(closure)};
    }
};

namespace views {
    inline constexpr chunk_by_view_adaptor chunk_by{};
}

