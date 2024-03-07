#include "cpp11.hpp"
#include "cpp11/matrix.hpp"
#include "cpp11/doubles.hpp"
#include <boost/array.hpp>
#include <vector>
#include <set>
#include <algorithm>
#include <unordered_set>
#include <iostream>
#include <numeric>
#include <random>

using namespace cpp11;
namespace writable = cpp11::writable;
using namespace std;

// It is slightly worse than rowSums() in R
[[cpp11::register]] writable::doubles row_sum(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = as_cpp<vector<double>>(tb[0]);

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = as_cpp<vector<double>>(tb[i]);
            for (int j = 0; j < nrow; j++)
            {
                double value = col[j];
                out_vec[j] += value;
            }
        }
        return out_vec;
    }
}

// Deprecated
// [[cpp11::register]] writable::doubles row_max(data_frame tb)
// {
//     int nrow = tb.nrow();
//     int ncol = tb.size();
//     std::vector<std::vector<double>> mat(nrow, vector<double>(ncol, 0));

//     for (int i = 0; i < ncol; i++)
//     {
//         writable::doubles col = as_cpp<vector<double>>(tb[i]);

//         for (int j = 0; j < nrow; j++)
//         {
//             double val = col[j];
//             mat[j][i] = val;
//         }
//     }

//     writable::doubles out_vec;

//     for (auto row : mat)
//     {
//         double out = *max_element(row.begin(), row.end());
//         out_vec.push_back(out);
//         out = 0;
//     }

//     return out_vec;
// }

[[cpp11::register]] writable::doubles row_max(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = as_cpp<vector<double>>(tb[0]);

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = as_cpp<vector<double>>(tb[i]);
            for (int j = 0; j < nrow; j++)
            {
                double value = col[j];
                if (out_vec[j] < value)
                {
                    out_vec[j] = value;
                }
            }
        }
        return out_vec;
    }
}

[[cpp11::register]] writable::doubles row_min(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = as_cpp<vector<double>>(tb[0]);

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = as_cpp<vector<double>>(tb[i]);
            for (int j = 0; j < nrow; j++)
            {
                double value = col[j];
                if (out_vec[j] > value)
                {
                    out_vec[j] = value;
                }
            }
        }
        return out_vec;
    }
}

[[cpp11::register]] writable::integers row_unique(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    std::vector<std::vector<double>> mat(nrow, vector<double>(ncol, 0));

    for (int i = 0; i < ncol; i++)
    {
        writable::doubles col = as_cpp<vector<double>>(tb[i]);

        for (int j = 0; j < nrow; j++)
        {
            double val = col[j];
            mat[j][i] = val;
        }
    }

    writable::integers out_vec;

    for (auto row : mat)
    {
        sort(row.begin(), row.end());
        int out = std::unique(row.begin(), row.end()) - row.begin();
        out_vec.push_back(out);
        out = 0;
    }

    return out_vec;
}

[[cpp11::register]] double s_sum(doubles vec)
{
    double out = 0;
    int n = vec.size();

    for (int i = 0; i < n; i++)
    {
        out += vec[i];
    }

    return out;
}

// Do not know how to construct data frame in cpp11
// [[cpp11::register]] data_frame transpose_tb(data_frame tb)
// {
//     int nrow = tb.nrow();
//     int ncol = tb.size();
//     std::vector<std::vector<double>> mat(nrow, vector<double>(ncol, 0));

//     for (int i = 0; i < ncol; i++)
//     {
//         writable::doubles col = as_cpp<vector<double>>(tb[i]);

//         for (int j = 0; j < nrow; j++)
//         {
//             double val = col[j];
//             mat[j][i] = val;
//         }
//     }

//     data_frame out_tb;

//     for (int i = 0; i < nrow; i++)
//     {
//         doubles out_row[ncol];
//         for (int j = 0; j < ncol; j++)
//         {
//             out_row[j] = mat[i][j];
//         }
//         out_tb.push_back(out_row);
//     }

//     return out_tb;
// }

// mean(na.rm = TRUE) spends about 2 times running time than s_mean
// mean(na.rm = TRUE) consistently occupies a lot of memeory
// The performance of mean() is not stable.
// s_mean() is preferred.
//
// > vec_tb1 <- runif(1e6) %>% c(rep(NA_real_, 1e6)) %>% tibble(a = .)
// > vec_tb2 <- runif(2e6) %>% tibble(a = .)
// > bench::mark(
// +   f1a = vec_tb1 %>% summarise(mean = mean(a, na.rm = TRUE)) %>% nrow(),
// +   f1b = vec_tb2 %>% summarise(mean = mean(a, na.rm = TRUE)) %>% nrow(),
// +   f2a = vec_tb1 %>% summarise(mean = mean(a)) %>% nrow(),
// +   f2b = vec_tb2 %>% summarise(mean = mean(a)) %>% nrow(),
// +   f3a = vec_tb1 %>% summarise(mean = s_mean(a)) %>% nrow(),
// +   f3b = vec_tb2 %>% summarise(mean = s_mean(a)) %>% nrow()
// + ) %>%
// +   arrange(median)
// # A tibble: 6 × 13
//   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
//   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
// 1 f2b          3.66ms   4.42ms    223.      1.84KB     0      112     0
// 2 f3b          6.81ms   8.82ms    112.      1.84KB     2.04    55     1
// 3 f3a           8.2ms   9.12ms    108.      1.84KB     0       55     0
// 4 f1a         11.09ms  12.99ms     72.9    34.33MB     0       37     0
// 5 f1b         16.61ms  20.55ms     45.2    45.78MB     2.26    20     1
// 6 f2a        274.44ms  287.8ms      3.47    1.84KB     0        2     0
// # ℹ 5 more variables: total_time <bch:tm>, result <list>, memory <list>,
// #   time <list>, gc <list>
[[cpp11::register]] double s_mean(doubles vec)
{
    vector<double> v = as_cpp<vector<double>>(vec);

    v.erase(std::remove_if(std::begin(v),
                           std::end(v),
                           [](const double &value)
                           { return std::isnan(value); }),
            std::end(v));

    double out = 0;
    int n = v.size();

    for (int i = 0; i < n; i++)
    {
        out += v[i];
    }

    out = out / n;

    return out;
}

[[cpp11::register]] double s_max(doubles vec)
{
    double out = vec[0];

    for (int i = 0; i < vec.size(); i++)
    {
        if (vec[i] > out)
        {
            out = vec[i];
        }
    }

    return out;
}

[[cpp11::register]] double dbl_sd(doubles dbl_vec)
{
    vector<double> vec = as_cpp<vector<double>>(dbl_vec);

    vec.erase(std::remove_if(std::begin(vec),
                             std::end(vec),
                             [](const double &value)
                             { return std::isnan(value); }),
              std::end(vec));

    double sum, out = 0;
    int n = vec.size();

    for (int i = 0; i < n; i++)
    {
        sum += vec[i];
    }

    double mean = sum / n;

    for (int i = 0; i < n; i++)
    {
        double rs = (vec[i] - mean);
        out += rs * rs;
    }

    out = sqrt(out / (n - 1));

    return out;
}

// Spend 1.5 to 4 times running time than n_distinct
// However, it does not require the memory, whereas n_distinct uses 1.37GB for a 1e8 vector
// Overall, n_distinct is preferred.
[[cpp11::register]] int n_distinct10k(doubles vec)
{
    int max_size = 1e4;

    if (vec.size() > max_size)
    {
        vector<double> large_vec = as_cpp<vector<double>>(vec);
        std::vector<double> v;

        // shuffle a large vector is too time consuming.
        // auto rng = std::default_random_engine{};
        // std::shuffle(large_vec.begin(), large_vec.end(), rng);

        for (int i = 0; i < max_size; ++i)
        {
            v.push_back(large_vec[i]);
        }

        sort(v.begin(), v.end());
        int out = std::unique(v.begin(), v.end()) - v.begin();

        return out;
    }
    else
    {
        vector<double> v = as_cpp<vector<double>>(vec);

        sort(v.begin(), v.end());
        int out = std::unique(v.begin(), v.end()) - v.begin();

        return out;
    }
}

// Assigning to a set is much slower
// [[cpp11::register]] int s_unique2(doubles vec)
// {
//     unordered_set<double> s;
//     s.insert(vec.begin(), vec.end());
//     s.erase(0);
//     return s.size();
// }

[[cpp11::register]] double s_median(doubles vec)
{
    vector<double> v = as_cpp<vector<double>>(vec);

    size_t n = v.size() / 2;
    size_t m = (v.size() - 1) / 2;
    if (n == m)
    {
        nth_element(v.begin(), v.begin() + n, v.end());
        return v[n];
    }
    else
    {
        nth_element(v.begin(), v.begin() + n, v.end());
        double median1 = v[n];
        nth_element(v.begin(), v.begin() + m, v.end());
        double median2 = v[m];
        return (median1 + median2) / 2.0;
    }
}
