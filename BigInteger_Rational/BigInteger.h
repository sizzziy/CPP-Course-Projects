#include <algorithm>
#include <complex>
#include <iostream>
#include <string>
#include <vector>

using std::endl;
using std::string;
using std::vector;

namespace detail {
const int mod = 998244353;
const int root = 31;
const int root_1 = 128805723;
const int root_pw = 1 << 23;
const int BASE_LEN = 2;
const int BASE = 100;
};  // namespace detail

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
  vector<int> nums = {0};  // reversed
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

  BigInteger& positive_sub_sorted(BigInteger b) {  // a >= b
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
    if (b == 0) return a;
    return gcd(b, a % b);
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

