// cklut_parquet.hpp — Parquet RowSource via Apache Arrow. Link: Arrow + Parquet.
#pragma once
#include "cklut_build.hpp"
#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/reader.h>

namespace cklut {

class ParquetRowSource : public RowSource {
public:
    explicit ParquetRowSource(std::string path) : path_(std::move(path)) { open(); }

    std::vector<std::string> columns() override {
        std::vector<std::string> names;
        for (const auto& f : schema_->fields()) names.push_back(f->name());
        return names;
    }
    void select(std::vector<int> dim_cols, std::vector<int> val_cols) override {
        dcols_ = std::move(dim_cols); vcols_ = std::move(val_cols);
    }
    void restart() override { rg_ = 0; row_ = 0; batch_.reset(); }

    bool next(std::vector<std::string>& dims, std::vector<double>& vals) override {
        if (!batch_ || row_ >= nrows_) { if (!load_next_group()) return false; }
        dims.resize(dcols_.size()); vals.resize(vcols_.size());
        for (std::size_t i=0;i<dcols_.size();++i) dims[i] = as_string(dcol_arr_[i].get(), row_);
        for (std::size_t i=0;i<vcols_.size();++i) vals[i] = as_double(vcol_arr_[i].get(), row_);
        ++row_;
        return true;
    }
private:
    void open() {
        std::shared_ptr<arrow::io::ReadableFile> infile;
        PARQUET_ASSIGN_OR_THROW(infile, arrow::io::ReadableFile::Open(path_));
        PARQUET_THROW_NOT_OK(parquet::arrow::OpenFile(infile, arrow::default_memory_pool(), &reader_));
        PARQUET_THROW_NOT_OK(reader_->GetSchema(&schema_));
        n_groups_ = reader_->num_row_groups();
    }
    bool load_next_group() {
        while (rg_ < n_groups_) {
            std::shared_ptr<arrow::Table> table;
            PARQUET_THROW_NOT_OK(reader_->ReadRowGroup(rg_++, &table));
            table_ = table->CombineChunks().ValueOrDie();   // one chunk per column
            nrows_ = table_->num_rows(); row_ = 0;
            dcol_arr_.clear(); vcol_arr_.clear();
            for (int c : dcols_) dcol_arr_.push_back(table_->column(c)->chunk(0));
            for (int c : vcols_) vcol_arr_.push_back(table_->column(c)->chunk(0));
            batch_ = table_;
            if (nrows_ > 0) return true;
        }
        return false;
    }
    static std::string as_string(const arrow::Array* a, int64_t i) {
        switch (a->type_id()) {
            case arrow::Type::STRING:  return static_cast<const arrow::StringArray*>(a)->GetString(i);
            case arrow::Type::LARGE_STRING: return static_cast<const arrow::LargeStringArray*>(a)->GetString(i);
            case arrow::Type::INT64:   return std::to_string(static_cast<const arrow::Int64Array*>(a)->Value(i));
            case arrow::Type::INT32:   return std::to_string(static_cast<const arrow::Int32Array*>(a)->Value(i));
            default: throw std::runtime_error("cklut: unsupported key column type");
        }
    }
    static double as_double(const arrow::Array* a, int64_t i) {
        switch (a->type_id()) {
            case arrow::Type::DOUBLE: return static_cast<const arrow::DoubleArray*>(a)->Value(i);
            case arrow::Type::FLOAT:  return static_cast<const arrow::FloatArray*>(a)->Value(i);
            case arrow::Type::INT64:  return double(static_cast<const arrow::Int64Array*>(a)->Value(i));
            default: throw std::runtime_error("cklut: unsupported value column type");
        }
    }
    std::string path_;
    std::unique_ptr<parquet::arrow::FileReader> reader_;
    std::shared_ptr<arrow::Schema> schema_;
    std::shared_ptr<arrow::Table> table_, batch_;
    std::vector<std::shared_ptr<arrow::Array>> dcol_arr_, vcol_arr_;
    std::vector<int> dcols_, vcols_;
    int n_groups_ = 0, rg_ = 0;
    int64_t nrows_ = 0, row_ = 0;
};

} // namespace cklut
