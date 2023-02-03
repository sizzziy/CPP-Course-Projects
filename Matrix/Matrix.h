#include <algorithm>
#include <complex>
#include <iostream>
#include <string>
#include <vector>

using std::endl;
using std::cout;
using std::string;
using std::vector;
using std::min;

namespace detail {
const int mod = 998244353;
const int root = 31;
const int root_1 = 128805723;
const int root_pw = 1 << 23;
const int BASE_LEN = 4;
const int BASE = 10000;
};    // namespace detail

class BigInteger;

bool operator>(const BigInteger& a, const BigInteger& b);
bool operator<=(const BigInteger& a, const BigInteger& b);
bool operator>=(const BigInteger& a, const BigInteger& b);
bool operator==(const BigInteger& a, const BigInteger& b);
bool operator!=(const BigInteger& a, const BigInteger& b);
BigInteger operator+(const BigInteger& a, const BigInteger& b);
BigInteger operator-(const BigInteger& a, const BigInteger& b);
BigInteger operator*(const BigInteger& a, const BigInteger& b);
BigInteger operator/(const BigInteger& a, const BigInteger& b);
BigInteger operator%(const BigInteger& a, const BigInteger& b);

class BigInteger {
 private:
    vector<int> nums = {0};    // reversed
    bool positive = true;

    BigInteger(const vector<int>& vnums) {
        positive = true;
        nums = vnums;
        if (nums.empty()) nums.push_back(0);
    }

    static int binpow_mod(int a, int b) {
        if (b == 0) return 1;
        if (b % 2 == 0) {
            int t = binpow_mod(a, b / 2);
            return 1ll * t * t % detail::mod;
        }
        return 1ll * binpow_mod(a, b - 1) * a % detail::mod;
    }

    static void fft(vector<int>& a, bool invert) {
        int n = static_cast<int>(a.size());
        for (int i = 1, j = 0; i < n; ++i) {
            int bit = n >> 1;
            for (; j >= bit; bit >>= 1) j -= bit;
            j += bit;
            if (i < j) std::swap(a[i], a[j]);
        }
        for (int len = 2; len <= n; len <<= 1) {
            int wlen = invert ? detail::root_1 : detail::root;
            for (int i = len; i < detail::root_pw; i <<= 1)
                wlen = (wlen * 1ll * wlen % detail::mod);
            for (int i = 0; i < n; i += len) {
                int w = 1;
                for (int j = 0; j < len / 2; ++j) {
                    int u = a[i + j],
                            v = (int)(a[i + j + len / 2] * 1ll * w % detail::mod);
                    a[i + j] = u + v < detail::mod ? u + v : u + v - detail::mod;
                    a[i + j + len / 2] = u - v >= 0 ? u - v : u - v + detail::mod;
                    w = (int)(w * 1ll * wlen % detail::mod);
                }
            }
        }
        if (invert) {
            int nrev = binpow_mod(n, detail::mod - 2);
            for (int i = 0; i < n; ++i)
                a[i] = static_cast<int>(a[i] * 1ll * nrev % detail::mod);
        }
    }

    static void normalize(vector<int>& a) {
        int carry = 0;
        for (size_t i = 0; i < a.size(); ++i) {
            a[i] += carry;
            carry = a[i] / detail::BASE;
            a[i] %= detail::BASE;
        }
        while (carry) {
            a.push_back(carry % detail::BASE);
            carry /= detail::BASE;
        }
    }

    vector<int> multiply(vector<int>& a, vector<int>& b) const {
        size_t n = 1;
        while (n < std::max(a.size(), b.size())) {
            n <<= 1;
        }
        n <<= 1;
        a.resize(n);
        b.resize(n);
        fft(a, 0);
        fft(b, 0);
        for (size_t i = 0; i < n; ++i) a[i] = a[i] * 1ll * b[i] % detail::mod;
        fft(a, 1);
        normalize(a);
        return a;
    }

    void make_same_length(BigInteger& b) {
        size_t new_size = std::max(nums.size(), b.nums.size());
        b.nums.resize(new_size);
        nums.resize(new_size);
    }

    void remove_leading_zeros() {
        while (nums.size() >= 2 && nums.back() == 0) {
            nums.pop_back();
        }
        if ((nums.size() == 1 && nums.back() == 0) || nums.empty()) positive = true;
    }

    BigInteger positive_add(const BigInteger& b) {
        if (b.nums.size() > nums.size()) nums.resize(b.nums.size());
        for (size_t i = 0; i < b.nums.size(); ++i) {
            nums[i] += b.nums[i];
        }
        normalize(nums);
        remove_leading_zeros();
        return *this;
    }

    BigInteger& positive_sub_sorted(BigInteger b) {    // a >= b
        make_same_length(b);
        for (size_t i = 0; i < b.nums.size(); ++i) {
            nums[i] -= b.nums[i];
            if (nums[i] < 0) {
                --nums[i + 1];
                nums[i] += detail::BASE;
            }
        }
        remove_leading_zeros();
        return *this;
    }

    BigInteger positive_sub(BigInteger b) {
        return (b.less_abs(*this) ? positive_sub_sorted(b)
                                                            : b.positive_sub_sorted(*this));
    }

    bool check_prod_by_digit(const BigInteger& b, int m) {
        // speed-boost. checking biggest digit first.
        int t = m * b.nums.back();
        if (nums.size() > b.nums.size()) {
            if (t > nums.back() * detail::BASE + nums[nums.size() - 2]) {
                return false;
            }
        } else {
            if (t > nums.back()) {
                return false;
            }
        }
        vector<int> prod(b.nums.size());
        for (size_t i = 0; i < b.nums.size(); ++i) prod[i] += m * b.nums[i];
        normalize(prod);
        if (prod.size() != nums.size()) return prod.size() < nums.size();
        for (int i = static_cast<int>(prod.size()) - 1; i >= 0; --i)
            if (prod[i] != nums[i]) return prod[i] < nums[i];
        return true;
    }

    int positive_divide_digit(const BigInteger& b) {
        int l = 0;
        int r = detail::BASE;
        while (r - l > 1) {
            int m = (l + r) / 2;
            check_prod_by_digit(b, m) ? l = m : r = m;
        }
        return l;
    }

    std::pair<BigInteger, BigInteger> positive_divide(BigInteger b) {
        b.positive = true;
        if (less_abs(b)) return {{0}, *this};
        int n = static_cast<int>(nums.size());
        vector<int> temp_nums(1);
        int i;
        for (i = n - 1; i >= 0; --i) {
            temp_nums.back() = nums[i];
            if (temp_nums.size() >= b.nums.size()) {
                reverse(temp_nums.begin(), temp_nums.end());
                if (BigInteger(temp_nums) >= b) {
                    break;
                }
                reverse(temp_nums.begin(), temp_nums.end());
            }
            temp_nums.push_back(0);
        }
        BigInteger temp(temp_nums);
        vector<int> ans_nums;
        for (; i >= 0; --i) {
            ans_nums.push_back(temp.positive_divide_digit(b));
            vector<int> prod(b.nums.size());
            for (size_t i = 0; i < b.nums.size(); ++i)
                prod[i] += ans_nums.back() * b.nums[i];
            normalize(prod);
            temp -= BigInteger(prod);
            if (i) {
                temp.nums.insert(temp.nums.begin(), nums[i - 1]);
            }
        }
        std::reverse(ans_nums.begin(), ans_nums.end());
        remove_leading_zeros();
        return {ans_nums, temp};
    }

    bool less_abs(const BigInteger& b) const {
        if (nums.size() != b.nums.size()) return nums.size() < b.nums.size();
        for (int i = static_cast<int>(nums.size()) - 1; i >= 0; --i) {
            if (nums[i] != b.nums[i]) return nums[i] < b.nums[i];
        }
        return false;
    }

 public:
    friend BigInteger gcd(BigInteger a, BigInteger b) {
        a.positive = true;
        b.positive = true;
        BigInteger res = 1;
        while (a > 0 && b > 0) {
            if (a.nums[0] % 2 == 0 && b.nums[0] % 2 == 0) {
                a /= 2;
                b /= 2;
                res *= 2;
            }
            else if (a.nums[0] % 2 == 0) {
                a /= 2;
            }
            else if (b.nums[0] % 2 == 0) {
                b /= 2;
            }
            else if (a < b) {
                b -= a;
            }
            else {
                a -= b;
            }
        }
        return res * std::max(a, b);
    }

    BigInteger() = default;

    BigInteger(int x) {
        positive = x >= 0;
        if (x < 0) {
            x = -x;
        }
        nums = {x};
        normalize(nums);
        if (nums.empty()) {
            nums.push_back(0);
        }
    }

    BigInteger(string s) {
        nums.clear();
        if (s.empty()) s.push_back('0');
        std::reverse(s.begin(), s.end());
        if (s.back() == '-') {
            positive = 0;
            s.pop_back();
        }
        std::reverse(s.begin(), s.end());
        while (!s.empty()) {
            int temp = 0, temp_pow = 1;
            int n = static_cast<int>(s.size());
            for (int i = 0; i < std::min(n, detail::BASE_LEN); ++i) {
                temp += (s.back() - '0') * temp_pow;
                temp_pow *= 10;
                s.pop_back();
            }
            nums.push_back(temp);
        }
        if (nums.empty()) nums.push_back(0);
        remove_leading_zeros();
    }

    void clear() {
        nums.clear();
        positive = true;
    }

    friend std::istream& operator>>(std::istream& in, BigInteger& a) {
        a.clear();
        string s;
        in >> s;
        a = BigInteger(s);
        return in;
    }

    string toString() const {
        string ans;
        for (int i = nums.size() - 1; i >= 0; --i) {
            string t = std::to_string(nums[i]);
            std::reverse(t.begin(), t.end());
            while (static_cast<int>(t.size()) < detail::BASE_LEN) {
                t.push_back('0');
            }
            std::reverse(t.begin(), t.end());
            ans += t;
        }
        std::reverse(ans.begin(), ans.end());
        while (static_cast<int>(ans.size()) >= 2 && ans.back() == '0')
            ans.pop_back();
        if (!positive) ans.push_back('-');
        std::reverse(ans.begin(), ans.end());
        if (ans == "-0") ans = "0";
        return ans;
    }

    friend std::ostream& operator<<(std::ostream& out, const BigInteger& a) {
        out << a.toString();
        return out;
    }

    explicit operator bool() const {
        return !((nums.size() == 1 && nums[0] == 0) || nums.empty());
    }

    BigInteger operator-() const {
        BigInteger temp = *this;
        temp.positive = !temp.positive;
        temp.remove_leading_zeros();
        return temp;
    }

    BigInteger& operator++() {
        *this += 1;
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger temp = *this;
        ++*this;
        return temp;
    }

    BigInteger& operator--() {
        *this -= 1;
        return *this;
    }

    BigInteger operator--(int) {
        BigInteger temp = *this;
        --*this;
        return temp;
    }

    friend bool operator<(const BigInteger& a, const BigInteger& b) {
        if (a.positive != b.positive)
            return a.positive < b.positive;
        else if (a.positive && b.positive)
            return a.less_abs(b);
        else
            return b.less_abs(a);
    }

    BigInteger& operator+=(const BigInteger& b) {
        if (positive == b.positive)
            *this = positive_add(b);
        else
            *this = positive_sub(b);
        remove_leading_zeros();
        return *this;
    }

    BigInteger& operator-=(const BigInteger& b) {
        remove_leading_zeros();
        *this += -b;
        return *this;
    }

    BigInteger& operator*=(const BigInteger& b) {
        vector<int> ba(nums.begin(), nums.end());
        vector<int> bb(b.nums.begin(), b.nums.end());
        nums = multiply(ba, bb);
        positive = !(positive ^ b.positive);
        remove_leading_zeros();
        return *this;
    }

    BigInteger& operator/=(int d) {
        int temp = 0;
        for (int i = static_cast<int>(nums.size()) - 1; i >= 0; --i) {
            int x = temp * detail::BASE + nums[i];
            temp = x % d;
            nums[i] = x / d;
        }
        positive = (positive == (d >= 0));
        remove_leading_zeros();
        return *this;
    }

    BigInteger& operator/=(const BigInteger& b) {
        bool posit = positive;
        *this = positive_divide(b).first;
        positive = (posit == b.positive);
        remove_leading_zeros();
        return *this;
    }

    BigInteger& operator%=(const BigInteger& b) {
        bool posit = positive;
        *this = positive_divide(b).second;
        positive = posit;
        remove_leading_zeros();
        return *this;
    }
};

bool operator>(const BigInteger& a, const BigInteger& b) { return b < a; }

bool operator==(const BigInteger& a, const BigInteger& b) {
    return !(a < b) && !(b < a);
}

bool operator!=(const BigInteger& a, const BigInteger& b) { return !(a == b); }

bool operator<=(const BigInteger& a, const BigInteger& b) { return !(a > b); }

bool operator>=(const BigInteger& a, const BigInteger& b) { return !(a < b); }

BigInteger operator+(const BigInteger& a, const BigInteger& b) {
    BigInteger copy = a;
    copy += b;
    return copy;
}

BigInteger operator-(const BigInteger& a, const BigInteger& b) {
    BigInteger copy = a;
    copy -= b;
    return copy;
}

BigInteger operator*(const BigInteger& a, const BigInteger& b) {
    BigInteger copy = a;
    copy *= b;
    return copy;
}

BigInteger operator/(const BigInteger& a, const BigInteger& b) {
    BigInteger copy = a;
    copy /= b;
    return copy;
}

BigInteger operator%(const BigInteger& a, const BigInteger& b) {
    BigInteger copy = a;
    copy %= b;
    return copy;
}

class Rational;

bool operator>(const Rational& a, const Rational& b);
bool operator<=(const Rational& a, const Rational& b);
bool operator>=(const Rational& a, const Rational& b);
bool operator==(const Rational& a, const Rational& b);
bool operator!=(const Rational& a, const Rational& b);
Rational operator+(const Rational& a, const Rational& b);
Rational operator-(const Rational& a, const Rational& b);
Rational operator*(const Rational& a, const Rational& b);
Rational operator/(const Rational& a, const Rational& b);

class Rational {
 private:
    BigInteger numenator = 0;
    BigInteger denominator = 1;
    void reduce() {
        if (denominator < 0) {
            denominator = -denominator;
            numenator = -numenator;
        }
        BigInteger d = gcd(numenator, denominator);
        numenator /= d;
        denominator /= d;
    }

 public:
    Rational() : numenator(0), denominator(1) {}

    Rational(const BigInteger& a) {
        numenator = a;
        denominator = 1;
    }
    Rational(int a) {
        numenator = a;
        denominator = 1;
    }

    friend bool operator<(const Rational& a, const Rational& b) {
        return a.numenator * b.denominator < a.denominator * b.numenator;
    }

    Rational operator-() const {
        Rational copy = *this;
        copy.numenator = -copy.numenator;
        copy.reduce();
        return copy;
    }

    Rational& operator+=(const Rational& b) {
        numenator = numenator * b.denominator + b.numenator * denominator;
        denominator = denominator * b.denominator;
        reduce();
        return *this;
    }

    Rational& operator-=(const Rational& b) {
        numenator = numenator * b.denominator - b.numenator * denominator;
        denominator = denominator * b.denominator;
        reduce();
        return *this;
    }

    Rational& operator*=(const Rational& b) {
        numenator = numenator * b.numenator;
        denominator = denominator * b.denominator;
        reduce();
        return *this;
    }

    Rational& operator/=(const Rational& b) {
        numenator = numenator * b.denominator;
        denominator = denominator * b.numenator;
        reduce();
        return *this;
    }

    friend std::istream& operator>>(std::istream& in, Rational& a) {
        string s;
        in >> s;
        a.numenator = BigInteger(s);
        a.denominator = 1;
        return in;
    }

    string toString() const {
        if (denominator == 1) return numenator.toString();
        return numenator.toString() + "/" + denominator.toString();
    }

    string asDecimal(size_t precision = 0) const {
        BigInteger temp_pow = 1;
        for (size_t i = 0; i < precision; ++i) temp_pow *= 10;
        BigInteger full = numenator * temp_pow * 10 / denominator;
        if ((full.toString().back() - '0') >= 5)
            full /= 10, full += (full >= 0);
        else
            full /= 10;
        string before_point;
        if (full < 0) {
            full = -full;
            before_point = '-';
        }
        before_point += (full / temp_pow).toString();
        string after_point = (full % temp_pow).toString();
        std::reverse(after_point.begin(), after_point.end());
        size_t ts = temp_pow.toString().size();
        while (after_point.size() < ts - 1) {
            after_point.push_back('0');
        }
        std::reverse(after_point.begin(), after_point.end());
        if (precision)
            return before_point + "." + after_point;
        else
            return before_point;
    }
    explicit operator double() const { return std::stod(asDecimal(50)); }
};

bool operator>(const Rational& a, const Rational& b) { return b < a; }

bool operator==(const Rational& a, const Rational& b) {
    return !(a < b) && !(b < a);
}

bool operator!=(const Rational& a, const Rational& b) { return !(a == b); }

bool operator<=(const Rational& a, const Rational& b) { return !(a > b); }

bool operator>=(const Rational& a, const Rational& b) { return !(a < b); }

Rational operator+(const Rational& a, const Rational& b) {
    Rational copy = a;
    copy += b;
    return copy;
}

Rational operator-(const Rational& a, const Rational& b) {
    Rational copy = a;
    copy -= b;
    return copy;
}

Rational operator*(const Rational& a, const Rational& b) {
    Rational copy = a;
    copy *= b;
    return copy;
}

Rational operator/(const Rational& a, const Rational& b) {
    Rational copy = a;
    copy /= b;
    return copy;
}

template<int N>
struct CompileErrorGenerator {
    static const bool res = CompileErrorGenerator<N + 2>::res;
};

template<>
struct CompileErrorGenerator<1> {
    static const bool res = true;
};

template<int N, int K>
struct IsPrimeHelper {
    static const bool res = N % K != 0 && IsPrimeHelper<N, K-1>::res;
};

template<int N>
struct IsPrimeHelper<N, 1> {
    static const bool res = true;
};

template<int N>
struct IsPrime {
    static const bool prime = IsPrimeHelper<N, std::min(N - 1, static_cast<int>(sqrt(N) + 1))>::res;
};

template<size_t N>
class Residue;

template<size_t N>
Residue<N> operator+(const Residue<N>& a, const Residue<N>& b);
template<size_t N>
Residue<N> operator-(const Residue<N>& a, const Residue<N>& b);
template<size_t N>
Residue<N> operator*(const Residue<N>& a, const Residue<N>& b);
template<size_t N>
Residue<N> operator/(const Residue<N>& a, const Residue<N>& b);

template<size_t N>
class Residue {
private:
    int x;

    static int binpow(int a, int b) {
        if (b == 0)
            return 1;
        if (b % 2 == 0) {
            int t = binpow(a, b / 2);
            return t * t % N;
        }
        int t = binpow(a, b - 1);
        return t * a % N;
    }

    static int reverse(int a) {
        return binpow(a, N - 2);
    }
public:
    explicit Residue<N>(int t) {
        int tN = static_cast<int>(N);
        x = (t % tN + tN) % tN;
    }

    Residue<N>() {
        x = 0;
    }

    Residue<N>& operator+=(const Residue<N>& b) {
        x = (x + b.x) % N;
        return *this;
    }

    Residue<N>& operator-=(const Residue<N>& b) {
        x = (x - b.x + N) % N;
        return *this;
    }

    Residue<N>& operator*=(const Residue<N>& b) {
        x = (x * b.x) % N;
        return *this;
    }
    
    Residue<N> operator-() {
        return Residue<N>(N - x);
    }

    Residue<N>& operator/=(const Residue<N>& b) {
        bool ok = CompileErrorGenerator<IsPrime<N>::prime>::res;
        x = ok * (x * Residue<N>::reverse(b.x)) % N;
        return *this;
    }

    friend bool operator==(const Residue<N>& a, const Residue<N>& b) {
        return a.x == b.x;
    }

    explicit operator int() const {
        return x;
    }
};

template<size_t N>
Residue<N> operator+(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> copy = a;
    copy += b;
    return copy;
}

template<size_t N>
Residue<N> operator-(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> copy = a;
    copy -= b;
    return copy;
}

template<size_t N>
Residue<N> operator*(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> copy = a;
    copy *= b;
    return copy;
}

template<size_t N>
Residue<N> operator/(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> copy = a;
    copy /= b;
    return copy;
}

template<size_t N>
bool operator!=(const Residue<N>& a, const Residue<N>& b) {
    return !(a == b);
}


template<size_t M, size_t N, typename Field=Rational>
class Matrix;

template<size_t M, size_t N, typename Field>
class Matrix {
private:
    vector<vector<Field>> a;

    static void substract(vector<Field>& a, const vector<Field>& b, Field c) {
        for (size_t i = 0; i < a.size(); ++i) {
            a[i] = a[i] - b[i] * c;
        }
    }

public:
    static vector<vector<Field>> make_forward_gauss(const vector<vector<Field>>& m, Field& d) {
        Field td = static_cast<Field>(1);
        vector<vector<Field>> res = m;
        vector<char> done(M);
        for (size_t i = 0; i < N; ++i) {
            int not_null_id = -1;
            for (size_t j = 0; j < M; ++j) {
                if (done[j] || res[j][i] == static_cast<Field>(0))
                    continue;
                not_null_id = j;
                break;
            }
            if (not_null_id == -1)
                continue;
            done[not_null_id] = true;
            for (size_t j = 0; j < M; ++j) {
                if (done[j] || res[j][i] == static_cast<Field>(0))
                    continue;
                Field c = res[j][i] / res[not_null_id][i];
                substract(res[j], res[not_null_id], c);
            }
        }

        vector<size_t> id(M, N + 1);
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                if (res[i][j] != static_cast<Field>(0)) {
                    id[i] = j;
                    break;
                }
            }
        }
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < M - 1; ++j) {
                if (id[j + 1] < id[j]) {
                    td = -td;
                    std::swap(id[j + 1], id[j]);
                    std::swap(res[j + 1], res[j]);
                }
            }
        }

        int last_done_id = -1;
        for (int i = 0; i < static_cast<int>(N); ++i) {
            int id = M;
            for (int j = static_cast<int>(M) - 1; j >= 0; --j) {
                if (res[j][i] != static_cast<Field>(0)) {
                    id = j;
                    break;
                }
            }
            if (id <= last_done_id)
                continue;
            if (id == M)
                continue;
            last_done_id = id;
            for (int j = id - 1; j >= 0; --j) {
                Field c = res[j][i] / res[id][i];
                substract(res[j], res[id], c);
            }
        }
        size_t trank = 0;
        for (size_t i = 0; i < M; ++i) {
            bool not_null = false;
            for (size_t j = 0; j < N && !not_null; ++j) {
                if (res[i][j] != static_cast<Field>(0))
                    not_null = true;
            }
            trank += not_null;
        }
        for (size_t i = 0; i < trank; ++i) {
            for (size_t j = 0; j < N; ++j) {
                if (res[i][j] != static_cast<Field>(0)) {
                    td *= res[i][j];
                    for (size_t k = 0; k < j; ++k) {
                        res[i][k] /= res[i][j];
                    }
                    for (size_t k = j + 1; k < N; ++k) {
                        res[i][k] /= res[i][j];
                    }
                    res[i][j] = static_cast<Field>(1);
                    break;
                }
            }
        }
        d = td;
        return res;
    }

    Matrix<M, N, Field>() {
        a = vector<vector<Field>>(M, vector<Field>(N));
        if (M == N) {
            for (size_t i = 0; i < N; ++i) {
                a[i][i] = static_cast<Field>(1);
            }
        }
    }

    Matrix<M, N, Field>(const vector<vector<Field>>& b) {
        a = b;
    }

    Matrix<M, N, Field>(const std::initializer_list<std::initializer_list<int>>& b) {
        a.resize(M, vector<Field>(N));
        int col = 0;
        for (auto& i : b) {
            int row = 0;
            for (auto& j : i) {
                a[col][row] = static_cast<Field>(j);
                row++;
            }
            ++col;
        }
    }

    Matrix<M, N, Field>(const std::initializer_list<std::initializer_list<Field>>& b) {
        a.resize(M, vector<Field>(N));
        int col = 0;
        for (auto& i : b) {
            int row = 0;
            for (auto& j : i) {
                a[col][row] = static_cast<Field>(j);
                row++;
            }
            ++col;
        }
    }

    Matrix<M, N, Field>(const vector<vector<int>>& b) {
        a = vector<vector<Field>>(M, vector<Field>(N));
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                a[i][j] = static_cast<Field>(b[i][j]);
            }
        }
    }

    vector<Field> getRow(unsigned index) {
        return a[index];
    }

    vector<Field> getColumn(unsigned index) {
        vector<Field> ans(M);
        for (size_t i = 0; i < M; ++i)
            ans[i] = a[i][index];
        return ans;
    }

    friend bool operator==(const Matrix& a, const Matrix& b) {
        return a.a == b.a;
    }

    Matrix<M, N, Field>& operator+=(const Matrix& b) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                a[i][j] += b[i][j];
            }
        }
        return *this;
    }

    Matrix<M, N, Field>& operator-=(const Matrix& b) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                a[i][j] -= b[i][j];
            }
        }
        return *this;
    }

    const vector<Field>& operator[](int index) const {
        return a[index];
    }

    vector<Field>& operator[](int index) {
        return a[index];
    }

    Matrix<M, N, Field>& operator*=(Field c) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                a[i][j] *= c;
            }
        }
        return *this;
    }

    template<size_t K>
    Matrix<M, K, Field>& operator*=(const Matrix<N, K, Field>& b) {
        *this = *this * b;
        return *this;
    }

    template<size_t K>
    Matrix<M, K, Field> operator*(const Matrix<N, K, Field>& b) const {
        vector<vector<Field>> t(M, vector<Field>(K));
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < K; ++j) {
                for (size_t k = 0; k < N; ++k) {
                    t[i][j] += a[i][k] * b[k][j];
                }
            }
        }
        return Matrix<M, K, Field>(t);
    }

    Field det() const {
        static_assert(N == M);
        Field det = static_cast<Field>(1);
        vector<vector<Field>> t = Matrix<M, N, Field>::make_forward_gauss(a, det);
        Field ans = t[0][0];
        for (size_t i = 1; i < N; ++i)
            ans *= t[i][i];
        return ans * det;
    }

    Field trace() const {
        static_assert(N == M);
        Field ans = static_cast<Field>(0);
        for (size_t i = 0; i < N; ++i) {
            ans += a[i][i];
        }
        return ans;
    }

    Matrix<N, M, Field> transposed() const {
        Matrix<N, M, Field> ans;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                ans[i][j] = a[j][i];
            }
        }
        return ans;
    }

    size_t rank() const {
        Field det = static_cast<Field>(1);
        vector<vector<Field>> t = Matrix<M, N, Field>::make_forward_gauss(a, det);
        size_t ans = 0;
        for (size_t i = 0; i < M; ++i) {
            bool not_null = false;
            for (size_t j = 0; j < N && !not_null; ++j) {
                if (t[i][j] != static_cast<Field>(0))
                    not_null = true;
            }
            ans += not_null;
        }
        return ans;
    }

    Matrix<M, N, Field> inverted() const {
        vector<vector<Field>> t = a;
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                t[i].push_back(static_cast<Field>(i == j));
            }
        }
        Field det;
        t = Matrix<M, 2 * N, Field>::make_forward_gauss(t, det);
        vector<vector<Field>> d(M);
        for (size_t i = 0; i < M; ++i) {
            d[i] = vector<Field>(t[i].begin() + N, t[i].end());
        }
        return Matrix<M, N, Field>(d);
    }

    void invert() {
        *this = inverted();
    }
};

template<size_t N, size_t M, typename Field = Rational>
bool operator!=(const Matrix<N, M, Field>& a, const Matrix<N, M, Field>& b) {
    return !(a == b);
}

template<size_t M, size_t N, typename Field=Rational>
Matrix<M, N, Field> operator+(const Matrix<M, N, Field>& a, const Matrix<M, N, Field>& b) {
    Matrix copy = a;
    copy += b;
    return copy;
}

template<size_t M, size_t N, typename Field=Rational>
Matrix<M, N, Field> operator-(const Matrix<M, N, Field>& a, const Matrix<M, N, Field>& b) {
    Matrix copy = a;
    copy -= b;
    return copy;
}

template<size_t M, size_t N, typename Field=Rational>
Matrix<M, N, Field> operator*(const Matrix<M, N, Field>& a, Field c) {
    Matrix copy = a;
    copy *= c;
    return copy;
}

template<size_t M, size_t N, typename Field=Rational>
Matrix<M, N, Field> operator*(Field c, const Matrix<M, N, Field>& a) {
    Matrix copy = a;
    copy *= c;
    return copy;
}

template<size_t N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;

