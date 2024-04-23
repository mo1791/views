#include <ranges>

template <typename Object_t>
    requires(std::conjunction<std::is_object<Object_t>,
                              std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>>::value)
struct repeat_n_view : public std::ranges::view_interface<repeat_n_view<Object_t>>
{

public:
    struct iterator;
    struct sentinel;

public:
    friend struct iterator;
    friend struct sentinel;

public:
    constexpr repeat_n_view() noexcept
        requires std::default_initializable<Object_t>
    = default;

    constexpr repeat_n_view(const Object_t &data, std::ptrdiff_t count) noexcept
        requires std::copy_constructible<Object_t>
        : m_data{data}, m_count{count}
    {
    }

    constexpr repeat_n_view(Object_t &&data, std::ptrdiff_t count) noexcept
        requires std::move_constructible<Object_t>
        : m_data{std::move(data)}, m_count{count}
    {
    }

    template <template <typename...> class Tuple, typename... Args>
    constexpr repeat_n_view(std::piecewise_construct_t,
                            const Tuple<Args...> &tuple,
                            std::ptrdiff_t count) noexcept
        requires(std::constructible_from<Object_t, Args...>)
        : m_data{std::make_from_tuple(tuple)}
        , m_count{count}
    {
    }

public:
    constexpr auto size() const noexcept { return m_count; }

public:
    constexpr auto begin()       noexcept { return iterator{*this, m_count}; }
    constexpr auto begin() const noexcept { return iterator{*this, m_count}; }

    constexpr auto end()       noexcept { return sentinel{}; }
    constexpr auto end() const noexcept { return sentinel{}; }

private:
    Object_t m_data        = Object_t{};
    std::ptrdiff_t m_count = 0UL;
};


template <typename Object> repeat_n_view(Object) -> repeat_n_view<Object>;


template <typename Object_t>
    requires(std::conjunction<std::is_object<Object_t>,
                              std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>>::value)
struct repeat_n_view<Object_t>::sentinel
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(const sentinel&,
                                     const sentinel& ) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(const sentinel&,
                                     const iterator &rhs) noexcept -> bool {
        return (rhs.m_cursor == 0UL);
    }
};

template <typename Object_t>
    requires(std::conjunction<std::is_object<Object_t>,
                              std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>>::value)
struct repeat_n_view<Object_t>::iterator
{

public:
    friend struct sentinel;


public:
    using value_type = Object_t;
    using reference  = std::add_lvalue_reference_t<Object_t>;
    using pointer    = std::add_pointer_t<Object_t>;

    using difference_type = std::ptrdiff_t;

    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::random_access_iterator_tag;


public:
    constexpr iterator() noexcept = default;

    constexpr iterator(const repeat_n_view &parent, difference_type cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_cursor{cursor}
    {
    }


public:
    constexpr auto operator*()       noexcept { return m_parent->m_data; }
    constexpr auto operator*() const noexcept { return m_parent->m_data; }

  public:
    constexpr auto operator[](difference_type index) noexcept {
        return *(*this + index);
    }

    constexpr auto operator[](difference_type index) const noexcept {
        return *(*this + index);
    }


public:
    constexpr auto operator++() noexcept -> iterator &
    {
        --m_cursor;
        return *this;
    }
    constexpr auto operator--() noexcept -> iterator &
    {
        ++m_cursor;
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
        m_cursor -= step;
        return *this;
    }

    constexpr auto operator-=(difference_type step) noexcept -> iterator &
    {
        m_cursor += step;
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
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<repeat_n_view>> m_parent = nullptr;
    difference_type m_cursor = 0UL;
};


struct repeat_n_view_closure : public std::ranges::range_adaptor_closure<repeat_n_view_closure>
{
public:
    constexpr repeat_n_view_closure(std::ptrdiff_t count) noexcept
        : m_count{count}
    {
    }

public:
    template <typename Object_t>
        requires(std::conjunction<std::is_object<Object_t>,
                                  std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>>::value)
    constexpr auto operator()(Object_t &&data) const noexcept 
    {
        return repeat_n_view{ std::forward<Object_t>(data), m_count };
    }

private:
    std::ptrdiff_t m_count;
};


struct repeat_n_view_adaptor
{
public:
    template <typename Object_t>
        requires(std::conjunction<std::is_object<Object_t>,
                                  std::bool_constant<std::same_as<Object_t, std::remove_cv_t<Object_t>>>>::value)
    constexpr auto operator()(Object_t &&data, std::ptrdiff_t count) const noexcept
    {
        return repeat_n_view{ std::forward<Object_t>(data), count };
    }

    constexpr auto operator()(std::ptrdiff_t count) const noexcept
    {
        return repeat_n_view_closure{ count };
    }
};

namespace views {
    inline constexpr repeat_n_view_adaptor repeat_n{};
}
