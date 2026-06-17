// cklut_parquet_typed.hpp — Parquet source for the typed (mixed-type) builder,
// via Apache Arrow. Link: Arrow + Parquet. Keys and values are read as strings
// and parsed by the builder according to each value column's declared ValType
// (so a value column may be double/float/int/long/logical/string regardless of
// the Parquet physical type, as long as it is convertible).
//
// NOTE: this header is only compiled when Arrow is available (CKLUT_PARQUET).
#pragma once
#include "cklut_build_typed.hpp"
#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/reader.h>

namespace cklut {

class ParquetTypedRowSource : public TypedRowSource {
public:
    explicit ParquetTypedRowSource(std::string path) : path_(std::move(path)) { open(); }

    std::vector<std::string> columns() override {
        std::vector<std::string> names;
        for (const auto& f : schema_->fields()) names.push_back(f->name());
        return names;
    }
    void select(std::vector<int> dim_cols, std::vector<int> val_cols) override {
        dcols_ = std::move(dim_cols); vcols_ = std::move(val_cols);
    }
    void restart() override { rg_ = 0; row_ = 0; batch_.reset(); }

    bool next(std::vector<std::string>& dims, std::vector<std::string>& vals) override {
        if (!batch_ || row_ >= nrows_) { if (!load_next_group()) return false; }
        dims.resize(dcols_.size()); vals.resize(vcols_.size());
        for (std::size_t i = 0; i < dcols_.size(); ++i) dims[i] = cell(dcol_arr_[i].get(), row_);
        for (std::size_t i = 0; i < vcols_.size(); ++i) vals[i] = cell(vcol_arr_[i].get(), row_);
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
            table_ = table->CombineChunks().ValueOrDie();
            nrows_ = table_->num_rows(); row_ = 0;
            dcol_arr_.clear(); vcol_arr_.clear();
            for (int c : dcols_) dcol_arr_.push_back(table_->column(c)->chunk(0));
            for (int c : vcols_) vcol_arr_.push_back(table_->column(c)->chunk(0));
            batch_ = table_;
            if (nrows_ > 0) return true;
        }
        return false;
    }
    // Render any supported Arrow cell to a string (NA/null -> "NA").
    static std::string cell(const arrow::Array* a, int64_t i) {
        if (a->IsNull(i)) return "NA";
        switch (a->type_id()) {
            case arrow::Type::STRING:       return static_cast<const arrow::StringArray*>(a)->GetString(i);
            case arrow::Type::LARGE_STRING: return static_cast<const arrow::LargeStringArray*>(a)->GetString(i);
            case arrow::Type::BOOL:         return static_cast<const arrow::BooleanArray*>(a)->Value(i) ? "TRUE" : "FALSE";
            case arrow::Type::INT64:        return std::to_string(static_cast<const arrow::Int64Array*>(a)->Value(i));
            case arrow::Type::INT32:        return std::to_string(static_cast<const arrow::Int32Array*>(a)->Value(i));
            case arrow::Type::DOUBLE:       { char b[32]; std::snprintf(b,sizeof b,"%.17g", static_cast<const arrow::DoubleArray*>(a)->Value(i)); return b; }
            case arrow::Type::FLOAT:        { char b[32]; std::snprintf(b,sizeof b,"%.9g",  (double)static_cast<const arrow::FloatArray*>(a)->Value(i)); return b; }
            case arrow::Type::DICTIONARY: {
                auto da = static_cast<const arrow::DictionaryArray*>(a);
                auto idx = std::static_pointer_cast<arrow::Int32Array>(da->indices());
                return cell(da->dictionary().get(), idx->Value(i));
            }
            default: throw std::runtime_error("cklut: unsupported Parquet column type");
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
