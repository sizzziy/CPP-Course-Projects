#include <memory>
#include <algorithm>

template<size_t N>
class alignas(max_align_t) StackStorage {
private:
    char buffer[N];
    void* pointer;
    size_t left = N;

public:
    StackStorage() noexcept : pointer(buffer) {}

    StackStorage(const StackStorage& other) = delete;

    StackStorage operator=(const StackStorage& other) = delete;

    char* allocate(size_t n) {
        if (__builtin_expect(std::align(alignof(max_align_t), n, pointer, left) != nullptr, 1)) {
            char* result = reinterpret_cast<char*>(pointer);
            pointer = (char*)pointer + n;
            left -= n;
            return result;
        }
        throw std::bad_alloc();
    }

    char* get_pointer() noexcept {
        return pointer;
    }
};

template<typename T, size_t N>
class StackAllocator {
private:
    StackStorage<N>* storage;

public:
    using value_type = T;

    template<typename U>
    struct rebind {
        typedef StackAllocator<U, N> other;
    };

    StackAllocator(StackStorage<N>& t_storage) noexcept {
        storage = &t_storage;
    }

    StackStorage<N>* get_storage() const noexcept {
        return storage;
    }

    template<typename U>
    StackAllocator(const StackAllocator<U, N>& alloc) noexcept : storage(alloc.get_storage()) { }

    T* allocate(size_t n) {
        T* result = reinterpret_cast<T*>(storage->allocate(n * sizeof(T)));
        return result;
    }

    void deallocate(T*, size_t) {}
};

template<typename T, typename Alloc = std::allocator<T>>
class List {
public:
    struct BaseNode {
        BaseNode* prev;
        BaseNode* next;

        BaseNode(): prev(this), next(this) {}
    };
    
    struct Node : BaseNode {
        T value;
    };

private:
    BaseNode fakeNode;
    size_t sz;
    typename Alloc::template rebind<Node>::other alloc; // allocator with <Node> not <T>
    
public:
    using AllocTraits = std::allocator_traits<typename Alloc::template rebind<Node>::other>;

    template<bool IsConst>
    struct common_iterator {
    private:
        std::conditional_t<IsConst, const BaseNode*, BaseNode*> ptr;

    public:
        using value_type = std::conditional_t<IsConst, const T, T>;
        using reference = std::conditional_t<IsConst, const T, T>&;
        using pointer = std::conditional_t<IsConst, const T, T>*;
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = typename std::pointer_traits<pointer>::difference_type;
        
        common_iterator(std::conditional_t<IsConst, const BaseNode*, BaseNode*> t) noexcept {
            ptr = t;
        }

        common_iterator(const common_iterator<false>& t) : ptr(t.ptr) {}

        bool operator==(const common_iterator& b) const noexcept {
            return ptr == b.ptr;
        }

        bool operator!=(const common_iterator& b) const noexcept {
            return ptr != b.ptr;
        }

        common_iterator& operator++() noexcept {
            ptr = ptr->next;
            return *this;
        }

        common_iterator operator++(int) noexcept {
            common_iterator temp(*this);
            ++*this;
            return temp;
        }

        common_iterator& operator--() noexcept {
            ptr = ptr->prev;
            return *this;
        }

        common_iterator operator--(int) noexcept {
            common_iterator temp(*this);
            --*this;
            return temp;
        }

        reference operator*() const noexcept {
            return (*static_cast<std::conditional_t<IsConst, const Node*, Node*>>(ptr)).value;
        }

        pointer operator->() const noexcept {
            return std::pointer_traits<pointer>::pointer_to(operator*());
        }

        friend List;
    };

    void copy_other_nodes(const List& other) {
        sz = 0;
        size_t cnt_added = 0;
        try {
            for (auto &x : other) {
                emplace_back(x);
                cnt_added++;
            }
        }
        catch (...) {
            for (size_t i = 0; i < cnt_added; ++i) {
                pop_back();
            }
            throw;
        }
    }

    template <class... Args>
    void emplace_back(const Args&... args) {
        ++sz;
        Node* new_node;
        try {
            new_node = AllocTraits::allocate(alloc, 1);
            AllocTraits::construct(alloc, std::addressof(new_node->value), args...);
        }
        catch (...) {
            AllocTraits::deallocate(alloc, new_node, 1);
            --sz;
            throw;
        }
        new_node->next = &fakeNode;
        new_node->prev = fakeNode.prev;
        fakeNode.prev = new_node;
        new_node->prev->next = new_node;
    }
    
public:
    using iterator = common_iterator<false>;
    using const_iterator = common_iterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator end() noexcept {
        return iterator(&fakeNode);
    }

    iterator begin() noexcept {
        return iterator(fakeNode.next);
    }
    
    const_iterator end() const noexcept {
        return const_iterator(&fakeNode);
    }

    const_iterator begin() const noexcept {
        return const_iterator(fakeNode.next);
    }
    
    const_iterator cend() const noexcept {
        return const_iterator(&fakeNode);
    }

    const_iterator cbegin() const noexcept {
        return const_iterator(fakeNode.next);
    }

    reverse_iterator rend() noexcept {
        return reverse_iterator(&fakeNode);
    }

    reverse_iterator rbegin() noexcept {
        return reverse_iterator(&fakeNode);
    }

    const_reverse_iterator rend() const noexcept {
        return const_reverse_iterator(&fakeNode);
    }

    const_reverse_iterator rbegin() const noexcept {
        return const_reverse_iterator(&fakeNode);
    }

    const_reverse_iterator crend() const noexcept {
        return const_reverse_iterator(&fakeNode);
    }

    const_reverse_iterator crbegin() const noexcept {
        return const_reverse_iterator(&fakeNode);
    }

    explicit List() noexcept : sz(0) {}
    
    explicit List(const Alloc& palloc) noexcept : sz(0), alloc(palloc) {}
    
    explicit List(size_t n) : List(n, std::allocator<T>()) {}

    explicit List(size_t n, const T& def) : List(n, def, std::allocator<T>()) {}

    explicit List(size_t n, const Alloc& palloc) : List(palloc) {
        for (size_t i = 0; i < n; ++i) {
            emplace_back();
        }
    }

    explicit List(size_t n, const T& def, const Alloc& palloc) : List(n, palloc) {
        for (size_t i = 0; i < n; ++i) {
            emplace_back(def);
        }
    }

    void push_back(const T& value) {
        insert(end(), value);
    }

    void pop_back() {
        erase(--end());
    }

    void push_front(const T& value) {
        insert(begin(), value);
    }

    void pop_front() {
        erase(begin());
    }

    size_t size() const noexcept {
        return sz;
    }

    void clear() noexcept {
        while (size() > 0) {
            pop_back();
        }
    }

    iterator insert(const_iterator pos, const T& x) {
        ++sz;
        --pos;
        Node* new_node;
        try {
            new_node = AllocTraits::allocate(alloc, 1);
            AllocTraits::construct(alloc, std::addressof(new_node->value), x);
        }
        catch (...) {
            AllocTraits::deallocate(alloc, new_node, 1);
            --sz;
            throw;
        }
        new_node->prev = const_cast<BaseNode*>(pos.ptr);
        new_node->next = pos.ptr->next;
        const_cast<BaseNode*>(pos.ptr)->next = new_node;
        new_node->next->prev = new_node;
        return iterator(new_node);
    }

    void erase(const_iterator pos) {
        --sz;
        Node* tnode = static_cast<Node*>(const_cast<BaseNode*>(pos.ptr));
        pos.ptr->prev->next = pos.ptr->prev->next->next;
        pos.ptr->next->prev = pos.ptr->next->prev->prev;
        AllocTraits::destroy(alloc, tnode);
        AllocTraits::deallocate(alloc, tnode, 1);
    }

    List(const List& other) 
        : alloc(AllocTraits::select_on_container_copy_construction(other.alloc))
    {
        copy_other_nodes(other);
    }

    List& operator=(const List& other) {
        if (this == &other)
            return *this;
        if (!AllocTraits::propagate_on_container_copy_assignment::value) {
            alloc = AllocTraits::select_on_container_copy_construction(other.alloc);
        }
        else {
            alloc = other.alloc;
        }
        Node* to_remove = static_cast<Node*>(fakeNode.next);
        size_t prev_sz = sz;
        copy_other_nodes(other);
        for (size_t i = 0; i < prev_sz; ++i) {
            Node* next = static_cast<Node*>(to_remove->next);
            AllocTraits::destroy(alloc, to_remove);
            AllocTraits::deallocate(alloc, to_remove, 1);
            to_remove = next;
        }
        return *this;
    }

    Alloc get_allocator() const noexcept {
        return alloc;
    }

    ~List() {
        clear();
    }
};
