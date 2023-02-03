#include <iostream>
#include <memory>
#include <vector>
#include <any>


struct BaseControlBlock {
    size_t shared_count;
    size_t weak_count;

    virtual void dealloc() {}
    virtual void destroy() {}

    BaseControlBlock(size_t sc, size_t wc) : shared_count(sc), weak_count(wc) {}
    virtual ~BaseControlBlock() = default;
};

template <typename T, typename Deleter, typename Alloc>
struct ControlBlockDirect : BaseControlBlock {
    T* ptr;
    Deleter deleter;
    Alloc alloc;
    
    ControlBlockDirect(T* ptr, size_t sc, size_t wc, Deleter del, Alloc alloc)
            : BaseControlBlock(sc, wc), ptr(ptr), deleter(del), alloc(alloc) {}

    void dealloc() override {
        using traits = typename std::allocator_traits<Alloc>::template rebind_traits<ControlBlockDirect>;
        typename traits::allocator_type new_alloc(alloc);
        new_alloc.deallocate(this, 1);
    }

    void destroy() override {
        deleter(ptr);
    }

    ~ControlBlockDirect() override {
        //destroy();
        deleter(ptr);
    }
};

template <typename T, typename U, typename Alloc>
struct ControlBlockMakeShared : BaseControlBlock {
    U obj;
    Alloc alloc;
    //T* ptr;

    void dealloc() override {
        using traits = typename std::allocator_traits<Alloc>::template rebind_traits<ControlBlockMakeShared>;
        typename traits::allocator_type new_alloc(alloc);
        new_alloc.deallocate(this, 1);
    }

    void destroy() override {
        using traits = typename std::allocator_traits<Alloc>::template rebind_traits<U>;
        typename traits::allocator_type new_alloc(alloc);
        traits::destroy(new_alloc, &obj);
    }
    
    template<typename... Args>
    ControlBlockMakeShared(size_t sc, size_t wc, const Alloc& alloc, Args&&... args)
        : BaseControlBlock(sc, wc), obj(std::forward<Args>(args)...), alloc(alloc) {}

    ~ControlBlockMakeShared() override {
        //destroy();
        //dealloc();
    }

};

template <typename U>
class WeakPtr;

template <typename T>
class SharedPtr {
private:
    
    BaseControlBlock* cb = nullptr;
    T* ptr = nullptr;

    template<typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(const Alloc&, Args&&...);

    template <typename U> 
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;

    template <typename U, typename Alloc>
    SharedPtr(ControlBlockMakeShared<T, U, Alloc>* cb) : cb(cb), ptr(&cb->obj) {}

    void erase_one() {
        if (!cb) return;
        --cb->shared_count;
        if (cb->weak_count == 0 && cb->shared_count == 0) {
            //delete cb;
            cb->destroy();
            cb->dealloc();
            cb = nullptr;
            ptr = nullptr;
        } else if (cb->shared_count == 0) {
            cb->destroy();
        }
    }

    SharedPtr(const WeakPtr<T>& other) : cb(other.cb), ptr(other.ptr) {
        cb->shared_count++;
    }

public:
    SharedPtr() {}

    template <typename U = T, typename Deleter = std::default_delete<T>, typename Alloc = std::allocator<T>>
    SharedPtr(U* obj = nullptr, Deleter del = std::default_delete<T>(), Alloc alloc = std::allocator<T>())
        : ptr(obj) {
        //static_assert(std::is_convertible_v<T, U>);
        using traits = typename std::allocator_traits<Alloc>::template rebind_traits<ControlBlockDirect<T, Deleter, Alloc>>;
        typename traits::allocator_type new_alloc(alloc);
        auto new_cb = traits::allocate(new_alloc, 1);
        new(new_cb) ControlBlockDirect<T, Deleter, Alloc>(obj, obj != nullptr, 0, del, new_alloc);
        //traits::construct(new_alloc, new_cb, obj, obj != nullptr, 0, del, new_alloc);
        cb = new_cb;
    }

    T* get() const {
        return ptr;
    }

    T& operator*() const {
        return *ptr;
    }

    T* operator->() const {
        return ptr;
    }

    size_t use_count() const {
        return cb->shared_count;
    }

    SharedPtr(const SharedPtr& other) : cb(other.cb), ptr(other.ptr) {
        if (cb)
            ++cb->shared_count;
    }

    template <typename U>
    SharedPtr(const SharedPtr<U>& other) : cb(other.cb), ptr(other.ptr) {
        if (cb)
            ++cb->shared_count;
    }

    template <typename U>
    SharedPtr(SharedPtr<U>&& other) : cb(std::move(other.cb)), ptr(other.ptr) {
        other.ptr = nullptr;
        other.cb = nullptr;
    }

    void swap(SharedPtr& r) {
        std::swap(cb, r.cb);
        std::swap(ptr, r.ptr);
    }

    void reset() {
        //erase_one();
        SharedPtr<T>().swap(*this);
    }

    template<typename U>
    void reset(U* p) {
        //erase_one();
        SharedPtr<U>(p).swap(*this);
    }

    SharedPtr& operator=(const SharedPtr& other) {
        if (this == &other)
            return *this;
        //std::cout << "bebra1";
        erase_one();
        cb = other.cb;
        ptr = other.ptr;
        if (cb)
            ++cb->shared_count;
        return *this;
    }

    template <typename U>
    SharedPtr& operator=(const SharedPtr<U>& other) {
        //std::cout << "bebra2";
        erase_one();
        cb = other.cb;
        ptr = other.ptr;
        if (cb)
            ++cb->shared_count;
        return *this;
    }

    template <typename U>
    SharedPtr& operator=(SharedPtr<U>&& other) {
        erase_one();
        cb = std::move(other.cb);
        ptr = other.ptr;
        other.cb = nullptr;
        other.ptr = nullptr;
        //cb->shared_count++;
        return *this;
    }

    ~SharedPtr() {
        erase_one();
    }
};

template <typename U, typename Alloc, typename... Args>
SharedPtr<U> allocateShared(const Alloc& alloc, Args&&... args) {
    using traits = typename std::allocator_traits<Alloc>::template rebind_traits<ControlBlockMakeShared<U, U, Alloc>>;
    typename traits::allocator_type new_alloc(alloc);
    ControlBlockMakeShared<U, U, Alloc>* new_cb = traits::allocate(new_alloc, 1);
    traits::construct(new_alloc, new_cb, 1, 0, alloc, std::forward<Args>(args)...);
    return SharedPtr<U>(new_cb);
}

template <typename T, typename... Args>
SharedPtr<T> makeShared(Args&&... args) {
    return allocateShared<T>(std::allocator<T>(), std::forward<Args>(args)...);
}

template <typename T>
class WeakPtr {
private:
    BaseControlBlock* cb = nullptr;
    T* ptr = nullptr;

    template <typename U> 
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;

    void erase_one() {
        if (!cb) return;
        --cb->weak_count;
        if(cb->weak_count == 0 && cb->shared_count == 0) {
            cb->dealloc();
        }
    }
public:
    WeakPtr() {}

    template <typename U>
    WeakPtr(const SharedPtr<U>& other) : cb(other.cb), ptr(other.ptr) {
        if (cb)
            ++cb->weak_count;
    }

    WeakPtr(const WeakPtr& other) : cb(other.cb), ptr(other.ptr) {
        if (cb)
            ++cb->weak_count;
    }

    template <typename U>
    WeakPtr(const WeakPtr<U>& other) : cb(other.cb), ptr(other.ptr) {
        if (cb)
            ++cb->weak_count;
    }

    template <typename U>
    WeakPtr(WeakPtr<U>&& other) : cb(std::move(other.cb)), ptr(other.ptr) {
        other.ptr = nullptr;
        other.cb = nullptr;
    }

    size_t use_count() const {
        return cb->shared_count;
    }

    WeakPtr& operator=(const WeakPtr& other) {
        erase_one();
        cb = other.cb;
        ptr = other.ptr;
        if (cb)
            ++cb->weak_count;
        return *this;
    }

    template <typename U>
    WeakPtr& operator=(WeakPtr<U>&& other) {
        erase_one();
        cb = std::move(other.cb);
        ptr = other.ptr;
        other.cb = nullptr;
        other.ptr = nullptr;
        return *this;
    }

    bool expired() const {
        return cb && cb->shared_count == 0;
    }

    SharedPtr<T> lock() const {
        return expired() ? SharedPtr<T>() : SharedPtr<T>(*this);
    }

    ~WeakPtr() {
        erase_one();
    }
};
