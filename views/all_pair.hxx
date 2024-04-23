#include <ranges>




namespace details {
template <typename Type1, typename Type2> struct pair
{
  public:
    [[no_unique_address]] Type1 m_first;
    [[no_unique_address]] Type2 m_second;
};
} // namespace details


template <std::ranges::view range_t>
struct all_pair_view : public std::ranges::view_interface<all_pair_view<range_t>>
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

    using difference_type = std::ranges::range_difference_t<range_t>;

public:
    constexpr all_pair_view() noexcept
        requires std::default_initializable<range_t>
    = default;

    constexpr all_pair_view(range_t range) noexcept
        requires std::default_initializable<iterator_t>
        : m_view{std::move(range)}
    {
    }

public:
    constexpr auto size() noexcept
        requires(std::ranges::sized_range<range_t>)
    {
        return std::ranges::size(m_view);
    }
    constexpr auto size() const noexcept
        requires(std::ranges::sized_range<const range_t>)
    {
        return std::ranges::size(m_view);
    }


public:
    constexpr auto begin() noexcept
    {
        return outer_iterator{
                *this, 
                std::ranges::begin(m_view), 
                std::ranges::next(std::ranges::begin(m_view))};
    }
    constexpr auto begin() const noexcept
        requires std::ranges::range<const range_t>
    {
        return outer_iterator{ 
                *this,
                std::ranges::begin(m_view),
                std::ranges::next(std::ranges::begin(m_view)) };
    }

    constexpr auto end()       noexcept { return outer_sentinel{}; }
    constexpr auto end() const noexcept
        requires std::ranges::range<const range_t>
    {
        return outer_sentinel{};
    }

private:
    range_t m_view = range_t{};
};


template <typename range_t>
all_pair_view(range_t &&) -> all_pair_view<std::views::all_t<range_t>>;


template <std::ranges::view range_t>
struct all_pair_view<range_t>::outer_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(outer_sentinel const &,
                                     outer_sentinel const &) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(outer_sentinel const &,
                                     outer_iterator const &rhs) noexcept -> bool{
        return rhs.m_outer == std::ranges::end(rhs->m_view);
    }
};

template <std::ranges::view range_t>
struct all_pair_view<range_t>::outer_iterator
{
public:
    struct inner_view;

public:
    friend struct outer_sentinel;

public:
    using iterator_t = std::ranges::iterator_t<range_t>;
    using sentinel_t = std::ranges::sentinel_t<range_t>;

    using value_type = inner_view;
    using reference  = inner_view;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;

public:
    constexpr outer_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;

    constexpr outer_iterator(const all_pair_view &parent, iterator_t current, iterator_t next) noexcept
        : m_parent{std::addressof(parent)}, m_outer{std::move(current)}, m_inner{std::move(next)}
    {
    }

public:
    constexpr auto operator*() const noexcept {
        return inner_view{ m_parent, m_outer, m_inner };
    }
    constexpr auto operator*() noexcept {
        return inner_view{ m_parent, m_outer, m_inner };
    }

public:
    constexpr auto operator++() noexcept -> outer_iterator &
    {
        m_outer = std::ranges::next(std::move(m_outer));
        m_inner = std::ranges::begin(m_parent->m_view);

        return *this;
    }

    constexpr void operator++(int) noexcept
    {
        std::ranges::advance(*this, 1);
    }

public:
    [[nodiscard]]
    friend constexpr auto operator==(outer_iterator const &lhs,
                                     outer_iterator const &rhs) noexcept -> bool {
        return lhs.m_outer == rhs.m_outer && lhs.m_inner == rhs.m_outer;
    }

private:
    constexpr auto operator->() const noexcept { return m_parent; }
    constexpr auto operator->()       noexcept { return m_parent; }

private:
    std::add_pointer_t<std::add_const_t<all_pair_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_outer = iterator_t{};
    std::ranges::iterator_t<range_t> m_inner = iterator_t{};
};


template <std::ranges::view range_t>
struct all_pair_view<range_t>::outer_iterator::inner_view : public std::ranges::view_interface<inner_view>
{
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


    constexpr inner_view(const all_pair_view *parent, iterator_t outer_cursor,
                                                      iterator_t inner_cursor) noexcept
        : m_parent{parent}
        , m_outer{std::move(outer_cursor)}
        , m_inner{std::move(inner_cursor)}
    {
    }

public:
    constexpr auto begin() noexcept {
        return inner_iterator{ *m_parent, m_outer, m_inner };
    }

    constexpr auto begin() const noexcept {
        return inner_iterator{ *m_parent, m_outer, m_inner };
    }

    constexpr auto end()       noexcept { return inner_sentinel{}; }
    constexpr auto end() const noexcept { return inner_sentinel{}; }


private:
    std::add_pointer_t<std::add_const_t<all_pair_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_outer = iterator_t{};
    std::ranges::iterator_t<range_t> m_inner = iterator_t{};
};


template <std::ranges::view range_t>
struct all_pair_view<range_t>::inner_sentinel : public std::default_sentinel_t
{
public:
    [[nodiscard]]
    friend constexpr auto operator==(inner_sentinel const &,
                                     inner_sentinel const &) noexcept -> bool {
        return true;
    }

    [[nodiscard]]
    friend constexpr auto operator==(inner_sentinel const &,
                                     inner_iterator const &rhs) noexcept -> bool {
        return (rhs.m_inner == std::ranges::end(rhs->m_view));
    }
};

template <std::ranges::view range_t>
struct all_pair_view<range_t>::inner_iterator
{

public:
    friend struct inner_sentinel;

public:
    using range_iterator_t = std::ranges::iterator_t<range_t>;
    using range_value_type = std::ranges::range_value_t<range_t>;
    using range_reference  = std::ranges::range_reference_t<range_t>;

    using difference_type = std::ranges::range_difference_t<range_t>;

    using value_type = details::pair<range_value_type, range_value_type>;
    using reference  = details::pair<range_reference, range_reference>;

    using iterator_category = std::input_iterator_tag;
    using iterator_concept  = std::input_iterator_tag;


public:
    constexpr inner_iterator() noexcept
        requires std::default_initializable<iterator_t>
    = default;


    constexpr inner_iterator(const all_pair_view &parent, iterator_t outer_cursor,
                                                          iterator_t inner_cursor) noexcept
        : m_parent{std::addressof(parent)}
        , m_outer{std::move(outer_cursor)}
        , m_inner{std::move(inner_cursor)}
    {
        if (m_inner == m_outer) {
            m_inner = std::ranges::next(std::move(m_inner));
        }
    }


public:
    constexpr auto operator*()       noexcept -> reference { return { *m_inner, *m_outer }; }
    constexpr auto operator*() const noexcept -> reference { return {*m_inner, *m_outer}; }


public:
    constexpr auto operator++() noexcept -> inner_iterator &
    {
        m_inner = std::ranges::next(std::move(m_inner));

        if (m_inner == m_outer) {
            m_inner = std::ranges::next(std::move(m_inner));
        }

        return *this;
    }

    constexpr void operator++(int) noexcept { std::ranges::advance(*this, 1); }


public:
    [[nodiscard]]
    friend constexpr auto operator==(inner_iterator const &lhs,
                                     inner_iterator const &rhs) noexcept -> bool {
        return lhs.m_inner == rhs.m_inner && lhs.m_outer == rhs.m_outer;
    }

private:
    constexpr auto operator->()       noexcept { return m_parent; }
    constexpr auto operator->() const noexcept { return m_parent; }


private:
    std::add_pointer_t<std::add_const_t<all_pair_view>> m_parent = nullptr;
    std::ranges::iterator_t<range_t> m_outer = iterator_t{};
    std::ranges::iterator_t<range_t> m_inner = iterator_t{};
};


struct all_pair_view_adaptor : public std::ranges::range_adaptor_closure<all_pair_view_adaptor>
{
    template <std::ranges::input_range range_t>
    constexpr auto operator()(range_t &&range) const noexcept {
        return all_pair_view{std::forward<range_t>(range)};
    }
};

namespace views {
    inline constexpr all_pair_view_adaptor all_pair{};
}
