#include <iostream>
#include <cstring>

class String;

String operator+(const String& a, const String& b);

class String {
private:
    size_t capacity = 0;
    size_t len = 0;
    char* str = nullptr;

    void increase_capacity() {
        capacity <<= 1;
        if (capacity == 0)
            capacity = 1;
        char* new_str = new char[capacity];
        if (str)
            std::memcpy(new_str, str, len);
        delete[] str;
        str = new_str;
    }

    size_t* KMP_algorithm(const String& substr) const {
        String new_str = substr + '\0' + *this;
        size_t n = new_str.length();
        size_t* prefix_function = new size_t[n];
        prefix_function[0] = 0;
        for (size_t i = 1; i < n; ++i) {
            int j = prefix_function[i-1];
            while (j > 0 && new_str.str[i] != new_str.str[j])
                j = prefix_function[j-1];
            if (new_str.str[i] == new_str.str[j])  ++j;
            prefix_function[i] = j;
        }
        return prefix_function;
    }
public:
    void clear() {
        len = 0;
        capacity = 0;
        str = nullptr;
    }

    String() = default;

    ~String() {
        delete[] str;
    }

    String(char new_char) {
        len = 1;
        capacity = len * 2;
        str = new char[1];
        str[0] = new_char;
    }

    String(const char* new_str) {
        len = std::strlen(new_str);
        capacity = len * 2;
        str = new char[capacity];
        std::memcpy(str, new_str, strlen(new_str));
    }

    String(size_t n, char c) {
        len = n;
        capacity = len * 2;
        str = new char[capacity];
        std::memset(str, c, len);
    }

    String(const String& b) {
        len = b.len;
        capacity = b.capacity;
        str = new char[capacity];
        std::memcpy(str, b.str, b.len);
    }

    void swap(String& s) {
        std::swap(str, s.str);
        std::swap(len, s.len);
        std::swap(capacity, s.capacity);
    }

    String& operator=(const String& b) {
        String copy = b;
        swap(copy);
        return *this;
    }

    char& operator[](size_t index) {
        return str[index];
    }

    const char& operator[](size_t index) const {
        return str[index];
    }

    char& front() {
        return str[0];
    }

    const char& front() const {
        return str[0];
    }

    char& back() {
        return str[len - 1];
    }

    const char& back() const {
        return str[len - 1];
    }

    size_t length() const {
        return len;
    }

    void push_back(char c) {
        if (len == capacity)
            increase_capacity();
        str[len] = c;
        len++;
    }

    void pop_back() {
        len--;
        str[len] = '\0';
    }

    String& operator+=(char c) {
        push_back(c);
        return *this;
    }

    String& operator+=(const String& s) {
        if (capacity - len >= s.len) {
            if (s.str)
                std::memcpy(str + len, s.str, s.len);
            len += s.len;
            return *this;
        }
        capacity += s.capacity;
        char* new_str = new char[capacity];
        if (str)
            std::memcpy(new_str, str, len);
        std::memcpy(new_str + len, s.str, s.len);
        len += s.len;
        delete[] str;
        str = new_str;
        return *this;
    }

    friend bool operator==(const String& a, const String& b) {
        if (a.len != b.len)
            return false;
        for (size_t i = 0; i < a.len; ++i) {
            if (a.str[i] != b.str[i])
                return false;
        }
        return true;
    }

    size_t find_impl(const String& substr, int from, int to, int step) const {
        if (substr.len > len)
            return len;
        size_t* prefix_function = KMP_algorithm(substr);
        for (int i = from; i != to; i += step) {
            if (prefix_function[i] == substr.len) {
                int t_ret = prefix_function[i];
                delete[] prefix_function;
                return i - 2 * t_ret;
            }
        }
        delete[] prefix_function;
        return len;
    }

    size_t find(const String& substr) const {
        return find_impl(substr, substr.len, substr.len + len + 1, 1);
    }

    size_t rfind(const String& substr) const {
        return find_impl(substr, substr.len + len, -1, -1);
    }

    String(char* begin, char* end) {
        clear();
        for (char* i = begin; i != end; ++i) {
            push_back(i[0]);
        }
    }

    String substr(const size_t index, const size_t count) const {
        if (count == 0)
            return String();
        return String(str + index, str + index + count);
    }

    bool empty() const {
        return len == 0;
    }

    friend std::ostream& operator<<(std::ostream& out, const String& s) {
        for (size_t i = 0; i < s.length(); ++i) {
            out << s[i];
        }
        return out;
    }
    
    friend std::istream& operator>>(std::istream& in, String& s) {
        s.len = 0;
        char c;
        in.get(c);
        while (isspace(c)) {
            in.get(c);
            if (!(in.get(c)))
                break;
        }
        while (!isspace(c)) {
            s.push_back(c);
            if (!(in.get(c)))
                break;
        }
        return in;
    }
};

String operator+(const String& a, const String& b) {
    String copy = a;
    copy += b;
    return copy;
}
