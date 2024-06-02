#include "cpp11.hpp"
#include "cpp11/matrix.hpp"
#include "cpp11/doubles.hpp"
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

[[cpp11::register]] cpp11::doubles_matrix<> df_to_mat(data_frame tb, double na)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles_matrix<> mat(nrow, ncol);

    for (int i = 0; i < ncol; i++)
    {
        col = tb[i];
        for (int j = 0; j < nrow; j++)
        {
            double val = col[j];
            if (std::isnan(val))
            {
                mat(j, i) = na;
            }
            else
            {
                mat(j, i) = val;
            }
        }
    }

    return mat;
}

[[cpp11::register]] writable::doubles row_max_dbl(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = tb[0];

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = tb[i];
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

[[cpp11::register]] writable::doubles row_min_dbl(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = tb[0];

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = tb[i];
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

[[cpp11::register]] writable::doubles row_sum_dbl(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = tb[0];

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = tb[i];
            for (int j = 0; j < nrow; j++)
            {
                double value = col[j];
                out_vec[j] += value;
            }
        }
        return out_vec;
    }
}

[[cpp11::register]] writable::doubles row_mean_dbl(data_frame tb)
{
    int nrow = tb.nrow();
    int ncol = tb.size();
    writable::doubles col;
    writable::doubles out_vec = tb[0];

    if (ncol == 1)
    {
        return out_vec;
    }
    else
    {
        for (int i = 1; i < ncol; i++)
        {
            col = tb[i];
            for (int j = 0; j < nrow; j++)
            {
                double value = col[j];
                out_vec[j] += value;
            }
        }
        for (int j = 0; j < nrow; j++)
        {
            out_vec[j] /= ncol;
        }
        return out_vec;
    }
}

[[cpp11::register]] writable::integers row_unique_dbl(data_frame tb)
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
