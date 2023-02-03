#include <iostream>
#include <memory>
#include <algorithm>
#include <vector>
#include <cmath>

template<typename T, typename Alloc = std::allocator<T>>
class List {
private:
    struct BaseNode {
        BaseNode* prev;
        BaseNode* next;

        BaseNode(): prev(this), next(this) {}
    };
    
    struct Node : BaseNode {
        T value;
        Node(const T& v): value(v) {}
        Node(T&& v): value(std::move(v)) {}


      /*template<typename... _Args>
        _Node*
        _M_create_node(_Args&&... __args)
        {
        auto __p = this->_M_get_node();
        auto& __alloc = _M_get_Node_allocator();
        __allocated_ptr<_Node_alloc_type> __guard{__alloc, __p};
        _Node_alloc_traits::construct(__alloc, __p->_M_valptr(),
                        std::forward<_Args>(__args)...);
        __guard = nullptr;
        return __p;
        }*/
    };

    template<typename... Args>
    Node* create_node(Args&&... args) {
        Node* new_node = AllocTraits::allocate(alloc, 1);
        AllocTraits::construct(alloc, std::addressof(new_node->value), std::forward<Args>(args)...);
        return new_node;
    }

    BaseNode fakeNode;
    size_t sz;
    typename Alloc::template rebind<Node>::other alloc; // allocator with <Node> not <T>
    
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

        common_iterator() noexcept {}
        
        explicit common_iterator(std::conditional_t<IsConst, const BaseNode*, BaseNode*> t) noexcept : ptr(t) {}

        template<bool C>
        common_iterator(const common_iterator<C>& t) noexcept : ptr(t.ptr) { }

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

        std::conditional_t<IsConst, const T&, T&> operator*() const noexcept {
            return (*static_cast<std::conditional_t<IsConst, const Node*, Node*>>(ptr)).value;
        }

        std::conditional_t<IsConst, const T*, T*> operator->() const noexcept {
            return std::pointer_traits<pointer>::pointer_to((*static_cast<std::conditional_t<IsConst, const Node*, Node*>>(ptr)).value);
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

    void push_back(T&& value) {
        insert(end(), std::move(value));
    }

    void pop_back() {
        erase(--end());
    }

    void push_front(const T& value) {
        insert(begin(), value);
    }

    void push_front(T&& value) {
        insert(begin(), std::move(value));
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
        return insert(pos, std::move(x));
    }

    iterator insert(const_iterator pos, T&& x) {
        ++sz;
        --pos;
        Node* new_node;
        try {
            new_node = AllocTraits::allocate(alloc, 1);
            AllocTraits::construct(alloc, new_node, std::move(x));
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

    template <class... Args>
    iterator emplace(const_iterator pos, Args&&... args) {
        ++sz;
        --pos;
        Node* new_node;
        try {
            new_node = create_node(std::forward<Args>(args)...);
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
        : alloc(AllocTraits::select_on_container_copy_construction(other.alloc)) {
        copy_other_nodes(other);
    }

    List(List&& other) : fakeNode(std::move(other.fakeNode)), sz(other.sz), alloc(std::move(other.alloc)) {
        if (other.sz) {
            fakeNode.prev->next = &fakeNode;
            fakeNode.next->prev = &fakeNode;
            other.fakeNode.next = other.fakeNode.prev = &other.fakeNode;
        }
        else {
            fakeNode.prev = &fakeNode;
            fakeNode.next = &fakeNode;
        }
        other.sz = 0;
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

    List& operator=(List&& other) {
		sz = other.sz;
        fakeNode = std::move(other.fakeNode);
		alloc = std::move(other.alloc);
        if (other.sz) {
            fakeNode.prev->next = &fakeNode;
            fakeNode.next->prev = &fakeNode;
            other.fakeNode.next = other.fakeNode.prev = &other.fakeNode;
        }
        else {
            fakeNode.prev = &fakeNode;
            fakeNode.next = &fakeNode;
        }
        other.sz = 0;
        return *this;
    }

    Alloc get_allocator() const noexcept {
        return alloc;
    }

    ~List() {
        clear();
    }
};


const size_t DEFAULT_BUCKET_COUNT = 100;

template<typename Key, typename Value,
	typename Hash = std::hash<Key>, typename Equal = std::equal_to<Key>,
	typename Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {
public:
	using NodeType = std::pair<const Key, Value>;

private:
	struct ListNode {
		NodeType kv;
		size_t cache;
        
        ListNode(ListNode&& other) : kv(std::move(const_cast<Key&>(other.kv.first)), std::move(other.kv.second)), cache(other.cache) {}
        ListNode(NodeType&& kv, size_t cache) : kv(std::move(const_cast<Key&>(kv.first)), std::move(kv.second)), cache(cache) {};
        ListNode(int, int) {};
        //ListNode(Key&& k, Value&& v, size_t cache) : kv(std::move(const_cast<Key&>(k)), std::move(v)), cache(cache) {};
	};
	List<ListNode> elements;
	std::vector<typename List<ListNode>::iterator> pointer_arr;

    size_t cur_bucket_count;
    float cur_max_load_factor = 1.f;
    Hash hasher;
    Equal comparator;
    typename Alloc::template rebind<NodeType>::other alloc; // allocator with <Node> not <T>
    using AllocTraits = std::allocator_traits<typename Alloc::template rebind<NodeType>::other>;


    template<bool IsConst>
    struct common_iterator {
    private:
        std::conditional_t<IsConst, typename List<ListNode>::const_iterator, typename List<ListNode>::iterator> ptr;

    public:
        using value_type = std::conditional_t<IsConst, const NodeType, NodeType>;
        using reference = std::conditional_t<IsConst, const NodeType, NodeType>&;
        using pointer = std::conditional_t<IsConst, const NodeType, NodeType>*;
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = typename std::pointer_traits<pointer>::difference_type;

        common_iterator() noexcept {}

        common_iterator(std::conditional_t<IsConst, typename List<ListNode>::const_iterator, typename List<ListNode>::iterator> t) : ptr(t) {}

        template<bool C>
        common_iterator(const common_iterator<C>& t) noexcept : ptr(t.ptr) { }

        bool operator==(const common_iterator& b) const noexcept {
            return ptr == b.ptr;
        }

        bool operator!=(const common_iterator& b) const noexcept {
            return ptr != b.ptr;
        }

        common_iterator& operator++() noexcept {
            ptr++;
            return *this;
        }

        common_iterator operator++(int) noexcept {
            common_iterator temp(*this);
            ++*this;
            return temp;
        }

        common_iterator& operator--() noexcept {
            ptr--;
            return *this;
        }

        common_iterator operator--(int) noexcept {
            common_iterator temp(*this);
            --*this;
            return temp;
        }

        reference operator*() const noexcept {
            return (*ptr).kv;
        }

        pointer operator->() const noexcept {
            return std::pointer_traits<pointer>::pointer_to((*ptr).kv);
        }

        friend UnorderedMap;
    };

    void try_rehash() {
        if (load_factor() > cur_max_load_factor) {
            rehash(cur_bucket_count * 2);
        }
    }
public:
    using iterator = common_iterator<false>;
    using const_iterator = common_iterator<true>;

    iterator end() {
        return iterator(elements.end());
    }

    iterator begin() {
        return iterator(elements.begin());
    }
    
    const_iterator end() const {
        return const_iterator(elements.end());
    }

    const_iterator begin() const {
        return const_iterator(elements.begin());
    }
    
    const_iterator cend() const {
        return const_iterator(elements.end());
    }

    const_iterator cbegin() const {
        return const_iterator(elements.begin());
    }

    UnorderedMap() : UnorderedMap(DEFAULT_BUCKET_COUNT) {}

    explicit UnorderedMap(size_t bucket_count,
                        const Hash& hash = Hash(),
                        const Equal& equal = Equal(),
                        const Alloc& alloc = Alloc())
                        : cur_bucket_count(bucket_count), hasher(hash), comparator(equal), alloc(alloc) {
        pointer_arr.resize(bucket_count, elements.end()); // maybe resize
    }

    UnorderedMap(size_t bucket_count,
               const Alloc& alloc )
              : UnorderedMap(bucket_count, Hash(), Equal(), alloc) {}

    UnorderedMap(size_t bucket_count,
               const Hash& hash,
               const Alloc& alloc )
              : UnorderedMap(bucket_count, hash, Equal(), alloc) {}
    
    explicit UnorderedMap(const Alloc& alloc)
                        : UnorderedMap(DEFAULT_BUCKET_COUNT, Hash(), Equal(), alloc) {}
    
    template <class InputIt>
    UnorderedMap(InputIt first, InputIt last,
                size_t bucket_count = DEFAULT_BUCKET_COUNT,
                const Hash& hash = Hash(),
                const Equal& equal = Equal(),
                const Alloc& alloc = Alloc()) {
        for (auto it = first; it != last; ++it) {
            insert(*it);
        }
    }

    template <class InputIt>
    UnorderedMap(InputIt first, InputIt last,
                size_t bucket_count,
                const Alloc& alloc )
                : UnorderedMap(first, last,
                    bucket_count, Hash(), Equal(), alloc) {}

    template <class InputIt>
    UnorderedMap(InputIt first, InputIt last,
                size_t bucket_count,
                const Hash& hash,
                const Alloc& alloc )
                : UnorderedMap(first, last,
                    bucket_count, hash, Equal(), alloc) {}
    
    UnorderedMap(const UnorderedMap& other) : cur_bucket_count(other.cur_bucket_count), 
                                        cur_max_load_factor(other.cur_max_load_factor),
                                        alloc(AllocTraits::select_on_container_copy_construction(other.alloc)) {
        pointer_arr.resize(cur_bucket_count, elements.end());
        for (auto it : other) {
            insert(it);
        }
    }
    
    UnorderedMap(const UnorderedMap& other, size_t cur_bucker_count) : cur_bucket_count(cur_bucker_count), 
                                        cur_max_load_factor(other.cur_max_load_factor),
                                        alloc(AllocTraits::select_on_container_copy_construction(other.alloc)) {
        pointer_arr.resize(cur_bucket_count, elements.end());
        for (auto& it : other) {
            insert(it);
        }
    }

    UnorderedMap(const UnorderedMap& other, const Alloc& alloc) : cur_bucket_count(other.cur_bucket_count), 
                                        cur_max_load_factor(other.cur_max_load_factor),
                                        alloc(alloc) {
        pointer_arr.resize(cur_bucket_count, elements.end());
        for (auto& it : other) {
            insert(it);
        }
    }

    UnorderedMap(UnorderedMap&& other) : elements(std::move(other.elements)), pointer_arr(std::move(other.pointer_arr)),
                                        cur_bucket_count(other.cur_bucket_count), 
                                        cur_max_load_factor(other.cur_max_load_factor),
                                        alloc(std::move(other.alloc)) {
        other.cur_bucket_count = 0;
    }

    UnorderedMap(UnorderedMap&& other, const Alloc& alloc) : cur_bucket_count(other.cur_bucket_count), 
                                        cur_max_load_factor(other.cur_max_load_factor),
                                        alloc(alloc), elements(std::move(other.elements)),
                                        pointer_arr(std::move(other.pointer_arr)) {
        other.cur_bucket_count = 0;
    }

    UnorderedMap& operator=(const UnorderedMap& other){
        if (this == &other)
            return *this;
        cur_bucket_count = other.cur_bucket_count; 
        cur_max_load_factor = other.cur_max_load_factor;
        alloc = AllocTraits::select_on_container_copy_construction(other.alloc);
        pointer_arr.resize(cur_bucket_count, elements.end());
        for (auto& it : other) {
            insert(it);
        }
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        cur_bucket_count = other.cur_bucket_count;
        cur_max_load_factor = other.cur_max_load_factor;
        alloc = std::move(other.alloc);
        elements = std::move(other.elements);
        pointer_arr.assign(cur_bucket_count, elements.end());
        other.cur_bucket_count = 0;
        return *this;
    }

    template<typename InputIterator>
    void insert(InputIterator first, InputIterator second) {
        for (auto it = first; it != second; ++it) {
            insert(*it);
        }
    }

    // TODO pair<iterator, bool>
    std::pair<iterator, bool> insert(const NodeType& value) {
        return emplace(std::move(NodeType(value)));
    }

    std::pair<iterator, bool> insert(NodeType&& value) {
        return emplace(std::move(const_cast<Key&>(value.first)), std::move(value.second));
    }

    Value& at(const Key& key) {
        size_t thash = Hash()(key);
        auto pointer = pointer_arr[thash % cur_bucket_count];
        if (pointer == elements.end())
            throw std::out_of_range("Unordered_map::at()");
        while (pointer != elements.end() && (*pointer).cache % cur_bucket_count == thash % cur_bucket_count) {
            if (Equal()((*pointer).kv.first, key)) {
                return (*pointer).kv.second;
            }
            ++pointer;
        }
        throw std::out_of_range("Unordered_map::at()");
    }

    const Value& at(const Key& key) const {
        size_t thash = Hash()(key);
        auto pointer = pointer_arr[thash % cur_bucket_count];
        if (pointer == 0)
            throw std::out_of_range("Unordered_map::at()");
        while (pointer != 0 && (*pointer).cache % cur_bucket_count == thash % cur_bucket_count) {
            if (Equal()((*pointer).kv.first, key)) {
                return (*pointer).kv.second;
            }
            ++pointer;
        }
        throw std::out_of_range("Unordered_map::at() const");
    }

    Value& operator[](const Key& key) {
        auto res = insert({key, Value()});
        return res.first->second;
    }

    size_t size() const {
        return elements.size();
    }

    template<typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        NodeType* new_node;
        new_node = AllocTraits::allocate(alloc, 1);
        try {
            AllocTraits::construct(alloc, new_node, std::forward<Args>(args)...);
        }
        catch (...) {
            AllocTraits::deallocate(alloc, new_node, 1);
            throw;
        }
        size_t thash = Hash()(new_node->first);
        if (pointer_arr[thash % cur_bucket_count] == elements.end()) {
            try {
                std::pair<Key, Value>&& t = {std::move(const_cast<Key&>(new_node->first)), std::move(new_node->second)};
                ListNode tnew_node{std::move(t), thash};
                elements.insert(elements.begin(), std::move(tnew_node));
            }
            catch(...) {
                return {elements.end(), false};
                throw;
            }
            pointer_arr[thash % cur_bucket_count] = elements.begin();
            try_rehash();
            return {iterator(elements.begin()), true};
        }
        else {
            auto pointer = pointer_arr[thash % cur_bucket_count];
            while (pointer != elements.end() && (*pointer).cache % cur_bucket_count == thash % cur_bucket_count) {
                if (Equal()((*pointer).kv.first, new_node->first)) {
                    return {iterator(pointer), false}; // already in map
                }
                ++pointer;
            }
            std::pair<Key, Value>&& t = {std::move(const_cast<Key&>(new_node->first)), std::move(new_node->second)};
            ListNode tnew_node{std::move(t), thash};
            elements.insert(pointer, std::move(tnew_node));
            try_rehash();
            return {iterator(pointer), true};
        }
    }

    void erase(iterator it) {
        size_t thash = Hash()((*it).first);
        auto pointer = pointer_arr[thash % cur_bucket_count];
        if ((*++pointer).cache % cur_bucket_count == thash % cur_bucket_count) {
            pointer_arr[thash % cur_bucket_count] = pointer;
        }
        else {
            pointer_arr[thash % cur_bucket_count] = elements.end();
        }
        elements.erase(it.ptr);
    }

    void erase(iterator first, iterator second) {
        std::vector<iterator> to_erase;
        for (auto it = first; it != second; ++it) {
            to_erase.push_back(it);
        }
        for (auto &it : to_erase) {
            erase(it);
        }
    }

    iterator find(const Key& key) {
        size_t thash = Hash()(key);
        if (pointer_arr[thash % cur_bucket_count] == elements.end()) {
            return end();
        }
        else {
            auto pointer = pointer_arr[thash % cur_bucket_count];
            while (pointer != elements.end() && (*pointer).cache % cur_bucket_count == thash % cur_bucket_count) {
                if (Equal()((*pointer).kv.first, key)) {
                    return iterator(pointer);
                }
                ++pointer;
            }
            return end();
        }
    }

    void clear() {
        while (size() > 0) {
            erase(begin());
        }
    }

    ~UnorderedMap() {
        clear();
    }

    float load_factor() const {
        return static_cast<float>(size()) / cur_bucket_count;
    }

    float max_load_factor() const {
        return cur_max_load_factor;
    }

    void max_load_factor(float ml) {
        cur_max_load_factor = ml;
    }
    // count -- number of buckets
    void rehash(size_t count) {
        size_t bucs = std::max(count * 2 + 10, static_cast<size_t>(std::ceil(size() / cur_max_load_factor)));
        if (bucs > pointer_arr.size()) {
            UnorderedMap new_empty(bucs);
            for (auto& i : *this) {
                new_empty.insert(std::move(i));
                //new_empty.emplace(i.first, i.second);
            }
            *this = std::move(new_empty);            
        }
    }

    // count -- number of elementss
    void reserve(size_t count) {
        if (count > size()) {
            rehash(std::ceil(count / max_load_factor()));
        }
    }
};
