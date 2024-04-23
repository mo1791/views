#include <functional>
#include <ranges>



namespace details
{
template <typename Functor_t, typename... Args>
concept regular_invocable =
    std::conjunction<::std::is_object<Functor_t>, ::std::bool_constant<::std::move_constructible<Functor_t>>,
                     ::std::bool_constant<::std::regular_invocable<Functor_t &, Args...>>>::value;
}

template <std::ranges::view range_t, details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
struct transform_view : public std::ranges::view_interface<transform_view<range_t, Functor_t>>
{

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;
    using size_type  = std::ranges::range_size_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

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
    constexpr transform_view() noexcept
        requires(std::conjunction<std::bool_constant<std::default_initializable<range_t>>,
                                  std::bool_constant<std::default_initializable<Functor_t>>>::value )
    = default;

    constexpr transform_view(range_t view, Functor_t closure) noexcept
        : m_view{std::move(view)}
        , m_closure{std::move(closure)}

    {
    }

public:
    constexpr auto operator[](difference_type index) const noexcept -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_view[index];
    }

    constexpr auto operator[](difference_type index) noexcept -> reference
        requires std::ranges::random_access_range<range_t>
    {
        return m_view[index];
    }

public:

    constexpr auto size() noexcept -> std::ranges::range_size_t<range_t>
        requires std::ranges::sized_range<range_t>
    {
        return std::ranges::size(m_view);
    }

    constexpr auto size() const noexcept 
        -> std::ranges::range_size_t<const range_t>
        requires std::ranges::sized_range<const range_t>
    {
        return std::ranges::size(m_view);
    }

public:
    constexpr auto begin()       noexcept { return iterator{*this, std::ranges::begin(m_view)}; }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_iterator{*this, std::ranges::begin(m_view)};
    }

    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return const_sentinel{};
    }


private:
    range_t   m_view    = range_t{};
    Functor_t m_closure = Functor_t{};
};


template <typename range_t, typename Functor_t>
transform_view(range_t &&, Functor_t) -> transform_view<std::views::all_t<range_t>, Functor_t>;



template <std::ranges::view range_t, details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
struct transform_view<range_t, Functor_t>::sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(sentinel const &,
                                     sentinel const &) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(sentinel const &,
                                     iterator const &rhs) noexcept -> bool {
        return ( rhs.m_cursor == std::ranges::end(rhs->m_cursor) );
    }
};


template <std::ranges::view range_t, details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
struct transform_view<range_t, Functor_t>::const_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const_sentinel const &,
                                     const_sentinel const &) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const_sentinel const &,
                                     const_iterator const &rhs) noexcept -> bool {
        return ( rhs.m_cursor == std::ranges::end(rhs->m_cursor) );
    }
};



template <std::ranges::view range_t, details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
struct transform_view<range_t, Functor_t>::iterator
{
public:
    friend struct sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = std::ranges::range_value_t<range_t>;
    using reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::random_access_iterator_tag;


public:
    constexpr iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr iterator(transform_view &parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {
    }

public:
    constexpr auto operator*() const noexcept {
        return std::invoke(std::ref(m_parent->m_closure), *m_cursor);
    }

    constexpr auto operator*() noexcept {
        return std::invoke(std::ref(m_parent->m_closure), *m_cursor);
    }


public:
    constexpr auto operator[](difference_type index) const noexcept -> reference
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
    constexpr auto operator++() noexcept -> iterator &
    {
        m_cursor = std::ranges::next(std::move(m_cursor));

        return *this;
    }

    [[nodiscard]] constexpr auto operator++(int) noexcept -> iterator
    {
        auto copy = *this;
        ++*this;

        return copy;
    }

    constexpr auto operator--() noexcept -> iterator &
        requires std::ranges::bidirectional_range<range_t>
    {
        m_cursor = std::ranges::prev(std::move(m_cursor));

        return *this;
    }

    [[nodiscard]] constexpr auto operator--(int) noexcept -> iterator
        requires std::ranges::bidirectional_range<range_t>
    {
        auto copy = *this;
        --*this;

        return copy;
    }

    constexpr auto operator+=(difference_type step) noexcept -> iterator &
        requires std::ranges::random_access_range<range_t>
    {
        m_cursor += step;

        return *this;
    }

    constexpr auto operator-=(difference_type step) noexcept -> iterator &
        requires std::ranges::random_access_range<range_t>
    {
        m_cursor -= step;

        return *this;
    }

public:
    [[nodiscard]]
    friend constexpr auto operator+(iterator lhs,
                                    difference_type step) noexcept -> iterator {
        return lhs += step;
    }

    [[nodiscard]]
    friend constexpr auto operator+(difference_type step,
                                    iterator rhs) noexcept -> iterator {
        return (rhs + step);
    }

    [[nodiscard]]
    friend constexpr auto operator-(iterator lhs,
                                    difference_type step) noexcept -> iterator {
        return lhs -= step;
    }

    [[nodiscard]]
    friend constexpr auto operator-(difference_type step,
                                    iterator rhs) noexcept -> iterator {
        return (rhs - step);
    }


public:
    [[nodiscard]]
    friend constexpr auto operator==(iterator const &lhs,
                                     iterator const &rhs) noexcept -> bool {
        return (lhs.m_cursor == rhs.m_cursor);
    }

private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }

private:
    std::add_pointer_t<transform_view> m_parent = nullptr;
    std::ranges::iterator_t<range_t>   m_cursor = iterator_t{};
};



template <std::ranges::view range_t, details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
struct transform_view<range_t, Functor_t>::const_iterator
{

public:
    friend struct const_sentinel;

public:
    using iterator_t = std::ranges::iterator_t<const range_t>;
    using sentinel_t = std::ranges::sentinel_t<const range_t>;

    using value_type = std::ranges::range_value_t<const range_t>;
    using reference  = std::ranges::range_reference_t<const range_t>;

    using difference_type = std::ranges::range_difference_t<const range_t>;

    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::random_access_iterator_tag;


public:
    constexpr const_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr const_iterator(const transform_view &parent, iterator_t cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{std::move(cursor)}
    {
    }

public:
    constexpr auto operator*() const noexcept
    {
        return std::invoke(std::ref(m_parent->m_closure), *m_cursor);
    }

    constexpr auto operator*() noexcept
    {
        return std::invoke(std::ref(m_parent->m_closure), *m_cursor);
    }

public:
    constexpr auto operator[](difference_type index) const noexcept -> reference
        requires std::ranges::random_access_range<const range_t>
    {
        return m_cursor[index];
    }

    constexpr auto operator[](difference_type index) noexcept -> reference
        requires std::ranges::random_access_range<const range_t>
    {
        return m_cursor[index];
    }


public:
    constexpr auto operator++() noexcept -> const_iterator &
    {
        m_cursor = std::ranges::next(std::move(m_cursor));

        return *this;
    }

    [[nodiscard]] constexpr auto operator++(int) noexcept -> const_iterator
    {
        auto copy = *this;
        std::ranges::advance( *this, 1 );

        return copy;
    }

    constexpr auto operator--() noexcept -> const_iterator &
        requires std::ranges::bidirectional_range<const range_t>
    {
        m_cursor = std::ranges::prev(std::move(m_cursor));

        return *this;
    }

    [[nodiscard]] constexpr auto operator--(int) noexcept -> const_iterator
        requires std::ranges::bidirectional_range<range_t>
    {
        auto copy = *this;
        std::ranges::advance( *this, -1 );

        return copy;
    }

    constexpr auto operator+=(difference_type step) noexcept -> const_iterator &
        requires std::ranges::random_access_range<const range_t>
    {
        m_cursor += step;

        return *this;
    }

    constexpr auto operator-=(difference_type step) noexcept -> const_iterator &
        requires std::ranges::random_access_range<const range_t>
    {
        m_cursor -= step;

        return *this;
    }

public:
    [[nodiscard]]
    friend constexpr auto operator+(const_iterator lhs,
                                    difference_type step) noexcept -> iterator {
        return lhs += step;
    }

    [[nodiscard]]
    friend constexpr auto operator+(difference_type step,
                                    const_iterator rhs) noexcept -> iterator {
        return (rhs + step);
    }

    [[nodiscard]]
    friend constexpr auto operator-(const_iterator lhs,
                                    difference_type step) noexcept -> iterator {
        return lhs -= step;
    }

    [[nodiscard]]
    friend constexpr auto operator-(difference_type step,
                                    const_iterator rhs) noexcept -> iterator {
        return (rhs - step);
    }

public:
    [[nodiscard]]
    friend constexpr auto operator==(const_iterator const &lhs,
                                     const_iterator const &rhs) noexcept -> bool {
        return (lhs.m_cursor == rhs.m_cursor);
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }

  private:
    std::add_pointer_t<std::add_const_t<transform_view>> m_parent = nullptr;
    std::ranges::iterator_t<std::add_const_t<range_t>>   m_cursor = iterator_t{};
};


template <typename Functor_t>
struct transform_view_closure : public std::ranges::range_adaptor_closure<transform_view_closure<Functor_t>>
{
public:
    constexpr transform_view_closure(Functor_t closure) noexcept
        : m_closure{std::move(closure)}
    {
    }

public:
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t &&view) const noexcept
    {
        return transform_view{std::forward<range_t>(view), std::move(m_closure)};
    }

private:
    Functor_t m_closure;
};

struct transform_view_adaptor
{
public:
    template <std::ranges::input_range range_t,
              details::regular_invocable<std::ranges::range_reference_t<range_t>> Functor_t>
    constexpr auto operator()(range_t &&view, Functor_t closure) const noexcept
    {
        return transform_view{std::forward<range_t>(view), std::move(closure)};
    }

    template <typename Functor_t>
    constexpr auto operator()(Functor_t closure) const noexcept
    {
        return transform_view_closure{std::move(closure)};
    }
};

namespace views {
    inline constexpr transform_view_adaptor transform{};
}
