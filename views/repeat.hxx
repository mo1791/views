#include <ranges>



template <typename Object_t, std::semiregular Bound_t = std::unreachable_sentinel_t>
    requires(std::conjunction<
             std::is_object<Object_t>, std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>,
             std::disjunction<std::bool_constant<std::integral<Bound_t>>,
                              std::bool_constant<std::same_as<Bound_t, std::unreachable_sentinel_t>>>>::value)
struct repeat_view : public std::ranges::view_interface<repeat_view<Object_t, Bound_t>>
{
public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;

public:
    constexpr repeat_view() noexcept
        requires std::default_initializable<Object_t>
    = default;

    constexpr repeat_view(const Object_t &data, Bound_t count = Bound_t()) noexcept
        requires std::copy_constructible<Object_t>
        : m_data{data}, m_count{count}
    {}

    constexpr repeat_view(Object_t &&data, Bound_t count = Bound_t()) noexcept
        requires std::move_constructible<Object_t>
        : m_data{std::move(data)}, m_count{count}
    {}

    template <template <typename...> class Tuple1, typename... Args1,
              template <typename...> class Tuple2, typename... Args2>
    constexpr repeat_view(std::piecewise_construct_t,
                          const Tuple1<Args1...> &tuple1,
                          const Tuple2<Args2...> &tuple2 = std::tuple{}) noexcept
        requires(std::conjunction<std::bool_constant<std::constructible_from<Object_t, Args1...>>,
                                  std::bool_constant<std::constructible_from<Bound_t, Args2...>>>::value)
        : m_data{std::make_from_tuple(tuple1)}
        , m_count{std::make_from_tuple(tuple2)}
    {}


public:
    constexpr auto begin()       noexcept { return iterator{*this}; }
    constexpr auto begin() const noexcept { return iterator{*this}; }

    constexpr auto end() noexcept
        requires(not std::same_as<Bound_t, std::unreachable_sentinel_t>)
    {
        return sentinel{};
    }

    constexpr auto end() const noexcept
        requires(not std::same_as<Bound_t, std::unreachable_sentinel_t>)
    {
        return sentinel{};
    }

    constexpr auto end() const noexcept { return std::unreachable_sentinel; }


private:
    Object_t m_data = Object_t{};
    Bound_t m_count = Bound_t{};
};


template <typename Object, typename Bound>
repeat_view(Object &&, Bound &&) -> repeat_view<Object, Bound>;




template <typename Object_t, std::semiregular Bound_t>
    requires(std::conjunction<
             std::is_object<Object_t>, std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>,
             std::disjunction<std::bool_constant<std::integral<Bound_t>>,
                              std::bool_constant<std::same_as<Bound_t, std::unreachable_sentinel_t>>>>::value)
struct repeat_view<Object_t, Bound_t>::sentinel
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const sentinel &,
                                     const sentinel &) noexcept -> bool {
        return true;
    }
    
    [[nodiscard]]
    friend constexpr auto operator==(const sentinel &,
                                     const iterator &rhs) noexcept -> bool {
        return (rhs.m_cursor == rhs->m_count);
    }
};



template <typename Object_t, std::semiregular Bound_t>
    requires(std::conjunction<
             std::is_object<Object_t>, std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>,
             std::disjunction<std::bool_constant<std::integral<Bound_t>>,
                              std::bool_constant<std::same_as<Bound_t, std::unreachable_sentinel_t>>>>::value)
struct repeat_view<Object_t, Bound_t>::iterator
{
public:
    friend struct sentinel;

public:
    using cursor_t = std::conditional_t<std::same_as<Bound_t, std::unreachable_sentinel_t>, std::ptrdiff_t, Bound_t>;

public:
    using value_type = Object_t;
    using reference  = std::add_lvalue_reference_t<Object_t>;
    using pointer    = std::add_pointer_t<Object_t>;

    using difference_type = std::ptrdiff_t;

    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::random_access_iterator_tag;


public:
    constexpr iterator() noexcept = default;

    constexpr iterator(const repeat_view &parent,
                       cursor_t cursor = cursor_t()) noexcept
        : m_parent{std::addressof(parent)}, m_cursor{cursor}
    {
    }

public:
    constexpr auto operator*()       noexcept { return m_parent->m_data; }
    constexpr auto operator*() const noexcept { return m_parent->m_data; }


public:
    constexpr auto operator[](difference_type index)       noexcept { return *(*this + index); }
    constexpr auto operator[](difference_type index) const noexcept { return *(*this + index); }


public:
    constexpr auto operator++() noexcept -> iterator &
    {
        ++m_cursor;
        return *this;
    }
    constexpr auto operator--() noexcept -> iterator &
    {
        --m_cursor;
        return *this;
    }

    constexpr auto operator++(int) noexcept -> iterator
    {
        auto copy = *this;
        ++*this;

        return copy;
    }
    constexpr auto operator--(int) noexcept -> iterator
    {
        auto copy = *this;
        --*this;

        return copy;
    }


public:
    constexpr auto operator+=(difference_type step) noexcept -> iterator &
    {
        if constexpr (not std::same_as<Bound_t, std::unreachable_sentinel_t>)
        {
            m_cursor += step;
        }

        return *this;
    }

    constexpr auto operator-=(difference_type step) noexcept -> iterator &
    {
        if constexpr (not std::same_as<Bound_t, std::unreachable_sentinel_t>)
        {
            m_cursor -= step;
        }

        return *this;
    }

public:
    friend constexpr auto operator+(difference_type step,
                                    iterator current) noexcept -> iterator {
        return current += step;
    }
    friend constexpr auto operator+(iterator current,
                                    difference_type step) noexcept -> iterator {
        return (step + current);
    }

    friend constexpr auto operator-(difference_type step,
                                    iterator current) noexcept -> iterator {
        return current -= step;
    }

    friend constexpr auto operator-(iterator current,
                                    difference_type step) noexcept -> iterator {
        return (step - current);
    }

    friend constexpr auto operator-(const iterator &lhs,
                                    const iterator &rhs) noexcept -> difference_type {
        return difference_type(lhs.m_cursor) - difference_type(rhs.m_cursor);
    }

private:
    constexpr auto operator->()       noexcept{ return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<repeat_view>> m_parent = nullptr;
    cursor_t m_cursor = cursor_t{};
};



struct repeat_view_closure
        : public std::ranges::range_adaptor_closure<repeat_view_closure>
{
public:
    template <typename Object_t, std::semiregular Bound_t = std::unreachable_sentinel_t>
        requires(std::conjunction<
                 std::is_object<Object_t>, std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>,
                 std::disjunction<std::bool_constant<std::integral<Bound_t>>,
                                  std::bool_constant<std::same_as<Bound_t, std::unreachable_sentinel_t>>>>::value)
    constexpr auto operator()(Object_t &&data, Bound_t count = Bound_t()) const noexcept
    {
        return repeat_view{std::forward<Object_t>(data), std::move(count)};
    }
};


namespace views {
    inline constexpr repeat_view_closure repeat{};
}

