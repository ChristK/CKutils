// cklut.hpp — dense, direct-address, memory-mapped lookup table, sharded.
// Header-only, C++17, cross-platform (POSIX + Windows), 64-bit.
#pragma once
#include <cstdint>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <string>
#include <string_view>
#include <vector>
#include <memory>
#include <unordered_map>
#include <fstream>
#include <stdexcept>
#include <type_traits>

#if defined(_WIN32)
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
#else
  #include <fcntl.h>
  #include <sys/mman.h>
  #include <sys/stat.h>
  #include <unistd.h>
#endif

namespace cklut {

// --------------------------- portable mmaps --------------------------------
namespace detail {

class FileMapRO {
public:
    explicit FileMapRO(const std::string& path) {
#if defined(_WIN32)
        file_ = ::CreateFileA(path.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr,
                              OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
        if (file_ == INVALID_HANDLE_VALUE) throw std::runtime_error("cklut: open RO: " + path);
        LARGE_INTEGER sz; if (!::GetFileSizeEx(file_, &sz)) { ::CloseHandle(file_); throw std::runtime_error("cklut: size"); }
        len_ = std::size_t(sz.QuadPart);
        if (len_) {
            map_ = ::CreateFileMappingA(file_, nullptr, PAGE_READONLY, 0, 0, nullptr);
            if (!map_) { ::CloseHandle(file_); throw std::runtime_error("cklut: CreateFileMapping RO"); }
            base_ = ::MapViewOfFile(map_, FILE_MAP_READ, 0, 0, 0);
            if (!base_) { ::CloseHandle(map_); ::CloseHandle(file_); throw std::runtime_error("cklut: MapViewOfFile RO"); }
        }
#else
        fd_ = ::open(path.c_str(), O_RDONLY);
        if (fd_ < 0) throw std::runtime_error("cklut: open RO: " + path);
        struct stat st{}; if (::fstat(fd_, &st) != 0) { ::close(fd_); throw std::runtime_error("cklut: fstat"); }
        len_ = std::size_t(st.st_size);
        if (len_) { base_ = ::mmap(nullptr, len_, PROT_READ, MAP_SHARED, fd_, 0);
                    if (base_ == MAP_FAILED) { ::close(fd_); throw std::runtime_error("cklut: mmap RO"); } }
#endif
    }
    ~FileMapRO() {
#if defined(_WIN32)
        if (base_) ::UnmapViewOfFile(base_); if (map_) ::CloseHandle(map_);
        if (file_ != INVALID_HANDLE_VALUE) ::CloseHandle(file_);
#else
        if (base_ && base_ != MAP_FAILED) ::munmap(base_, len_); if (fd_ >= 0) ::close(fd_);
#endif
    }
    FileMapRO(const FileMapRO&) = delete; FileMapRO& operator=(const FileMapRO&) = delete;
    const void* data() const { return base_; }
    std::size_t size() const { return len_; }

    // Async, best-effort hint: ask the OS to read the whole mapping ahead.
    // Non-blocking; pages stream into the page cache in the background.
    void willneed() const {
#if defined(_WIN32)
        if (base_ && len_) {
            WIN32_MEMORY_RANGE_ENTRY e{ base_, len_ };
            ::PrefetchVirtualMemory(::GetCurrentProcess(), 1, &e, 0);
        }
#elif defined(MADV_WILLNEED)
        if (base_ && len_) ::madvise(const_cast<void*>(base_), len_, MADV_WILLNEED);
#endif
    }
    // Blocking: touch one byte per page so the whole mapping is resident on
    // return. The returned checksum only exists to stop the loop being
    // optimized away; ignore it.
    std::uint64_t populate() const {
        std::uint64_t s = 0;
        if (base_ && len_) {
            const unsigned char* p = static_cast<const unsigned char*>(base_);
            for (std::size_t off = 0; off < len_; off += 4096) s += p[off];
            s += p[len_ - 1];
        }
        return s;
    }
private:
    void* base_ = nullptr; std::size_t len_ = 0;
#if defined(_WIN32)
    HANDLE file_ = INVALID_HANDLE_VALUE, map_ = nullptr;
#else
    int fd_ = -1;
#endif
};

class FileMapRW {
public:
    FileMapRW(const std::string& path, std::uint64_t bytes) : len_(std::size_t(bytes)) {
#if defined(_WIN32)
        file_ = ::CreateFileA(path.c_str(), GENERIC_READ | GENERIC_WRITE, 0, nullptr,
                              CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
        if (file_ == INVALID_HANDLE_VALUE) throw std::runtime_error("cklut: create RW: " + path);
        DWORD hi = DWORD(bytes >> 32), lo = DWORD(bytes & 0xFFFFFFFFu);
        map_ = ::CreateFileMappingA(file_, nullptr, PAGE_READWRITE, hi, lo, nullptr);
        if (!map_) { ::CloseHandle(file_); throw std::runtime_error("cklut: CreateFileMapping RW"); }
        base_ = ::MapViewOfFile(map_, FILE_MAP_WRITE, 0, 0, 0);
        if (!base_) { ::CloseHandle(map_); ::CloseHandle(file_); throw std::runtime_error("cklut: MapViewOfFile RW"); }
#else
        fd_ = ::open(path.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
        if (fd_ < 0) throw std::runtime_error("cklut: create RW: " + path);
        if (::ftruncate(fd_, off_t(bytes)) != 0) { ::close(fd_); throw std::runtime_error("cklut: ftruncate"); }
        base_ = ::mmap(nullptr, len_, PROT_READ | PROT_WRITE, MAP_SHARED, fd_, 0);
        if (base_ == MAP_FAILED) { ::close(fd_); throw std::runtime_error("cklut: mmap RW"); }
#endif
    }
    ~FileMapRW() {
#if defined(_WIN32)
        if (base_) { ::FlushViewOfFile(base_, 0); ::UnmapViewOfFile(base_); } if (map_) ::CloseHandle(map_);
        if (file_ != INVALID_HANDLE_VALUE) ::CloseHandle(file_);
#else
        if (base_ && base_ != MAP_FAILED) { ::msync(base_, len_, MS_SYNC); ::munmap(base_, len_); } if (fd_ >= 0) ::close(fd_);
#endif
    }
    FileMapRW(const FileMapRW&) = delete; FileMapRW& operator=(const FileMapRW&) = delete;
    void* data() const { return base_; }
private:
    void* base_ = nullptr; std::size_t len_ = 0;
#if defined(_WIN32)
    HANDLE file_ = INVALID_HANDLE_VALUE, map_ = nullptr;
#else
    int fd_ = -1;
#endif
};

inline void put_u32(std::vector<char>& b, std::uint32_t v){ for(int i=0;i<4;++i) b.push_back(char(v>>(8*i))); }
inline void put_u64(std::vector<char>& b, std::uint64_t v){ for(int i=0;i<8;++i) b.push_back(char(v>>(8*i))); }
inline std::uint32_t get_u32(const char*& p){ std::uint32_t v=0; for(int i=0;i<4;++i) v|=std::uint32_t(std::uint8_t(*p++))<<(8*i); return v; }
inline std::uint64_t get_u64(const char*& p){ std::uint64_t v=0; for(int i=0;i<8;++i) v|=std::uint64_t(std::uint8_t(*p++))<<(8*i); return v; }

// Fast unsigned division by a runtime-constant divisor (libdivide-style magic
// multiply-shift). Replaces the per-lookup `idx / rows_per_shard` (a ~20-30
// cycle hardware divide) with a multiply + shift. Falls back to plain division
// on compilers without 128-bit ints (e.g. MSVC).
struct FastDivU64 {
    std::uint64_t magic = 0;
    std::uint8_t  more  = 0;
    std::uint64_t d     = 1;
    static constexpr std::uint8_t ADD   = 0x40;
    static constexpr std::uint8_t SHIFT = 0x3F;

    void init(std::uint64_t divisor) {
        d = divisor ? divisor : 1;
#if defined(__SIZEOF_INT128__)
        std::uint32_t fl = 63 - __builtin_clzll(d);
        if ((d & (d - 1)) == 0) {                 // power of two -> pure shift
            magic = 0; more = std::uint8_t(fl);
        } else {
            __uint128_t num = (__uint128_t)1 << (64 + fl);
            std::uint64_t pm  = (std::uint64_t)(num / d);
            std::uint64_t rem = (std::uint64_t)(num - (__uint128_t)pm * d);
            std::uint64_t e = d - rem;
            if (e < ((std::uint64_t)1 << fl)) {
                more = std::uint8_t(fl);
            } else {
                pm += pm;
                std::uint64_t tr = rem + rem;
                if (tr >= d || tr < rem) pm += 1;
                more = std::uint8_t(fl | ADD);
            }
            magic = pm + 1;
        }
#else
        magic = 0; more = 0;                       // fallback marker: use n / d
#endif
    }
    std::uint64_t div(std::uint64_t n) const {
#if defined(__SIZEOF_INT128__)
        if (magic == 0) return n >> more;          // power-of-two path
        std::uint64_t q = (std::uint64_t)(((__uint128_t)magic * n) >> 64);
        if (more & ADD) { std::uint64_t t = ((n - q) >> 1) + q; return t >> (more & SHIFT); }
        return q >> more;
#else
        return n / d;
#endif
    }
};

inline std::string dir_of(const std::string& path){
    auto s = path.find_last_of("/\\"); return s==std::string::npos ? std::string() : path.substr(0,s);
}
inline std::string shard_path(const std::string& dir, const std::string& base, std::uint32_t i){
    char buf[24]; std::snprintf(buf,sizeof buf,".%04u.ckdat",i);
    return (dir.empty()? base : dir + "/" + base) + buf;
}
} // namespace detail

struct Dim { std::int64_t min; std::int64_t size; };   // index = key - min
static constexpr char kMetaMagic[8] = {'C','K','M','E','T','A','3','\0'};

// Build-time dimension description: numeric range OR string categories.
struct DimSpec {
    std::int64_t             min = 0, size = 0;
    std::vector<std::string> categories;   // non-empty => string dim (min=0, size=#cats)
    bool is_string() const { return !categories.empty(); }
};

// In-memory schema shared by Reader and Writer.
struct Schema {
    std::uint32_t n_values = 0;
    std::uint64_t n_rows = 0, rows_per_shard = 0;
    std::uint32_t n_shards = 0;
    std::string   base_name;                 // shard files: <base_name>.NNNN.ckdat
    std::vector<Dim> dims;
    std::vector<std::vector<std::string>> cats;   // per dim (empty if numeric)
    std::vector<std::uint64_t> strides;

    void compute_strides() {
        strides.assign(dims.size(), 0);
        std::uint64_t s = 1;
        for (std::size_t i = dims.size(); i-- > 0;) { strides[i] = s; s *= std::uint64_t(dims[i].size); }
    }
};

// Pick a shard size (rows) that is a multiple of the innermost run and <= max_bytes.
inline std::uint64_t rows_per_shard(const std::vector<Dim>& dims, std::uint32_t nv, std::uint64_t max_bytes) {
    const std::uint64_t inner = std::uint64_t(dims.back().size);
    const std::uint64_t row_bytes = std::uint64_t(nv) * sizeof(double);
    const std::uint64_t max_rows = max_bytes / row_bytes;
    if (max_rows < inner)
        throw std::runtime_error("cklut: one innermost run exceeds max shard bytes; raise the cap or put a smaller dimension last");
    return (max_rows / inner) * inner;
}

// ===========================================================================
// Reader
// ===========================================================================
template <std::size_t NV>
class Reader {
public:
    // warm = true eagerly loads the whole table into RAM before returning
    // (equivalent to constructing then calling prefetch(true)). Default false
    // keeps the lazy, demand-paged behavior.
    explicit Reader(const std::string& meta_path, bool warm = false) {
        std::ifstream f(meta_path, std::ios::binary);
        if (!f) throw std::runtime_error("cklut: open meta: " + meta_path);
        std::string buf((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
        const char* p = buf.data();
        if (std::memcmp(p, kMetaMagic, 8) != 0) throw std::runtime_error("cklut: bad meta magic");
        p += 8;
        detail::get_u32(p);                         // version
        std::uint32_t n_dims = detail::get_u32(p);
        sch_.n_values = detail::get_u32(p);
        detail::get_u32(p);                         // reserved
        if (sch_.n_values != NV) throw std::runtime_error("cklut: NV mismatch");
        sch_.n_rows        = detail::get_u64(p);
        sch_.rows_per_shard= detail::get_u64(p);
        sch_.n_shards      = detail::get_u32(p);
        std::uint32_t bl   = detail::get_u32(p);
        sch_.base_name.assign(p, p + bl); p += bl;

        sch_.dims.resize(n_dims); sch_.cats.resize(n_dims);
        dict_.resize(n_dims);
        for (std::uint32_t i=0;i<n_dims;++i){ sch_.dims[i].min=(std::int64_t)detail::get_u64(p); sch_.dims[i].size=(std::int64_t)detail::get_u64(p); }
        for (std::uint32_t i=0;i<n_dims;++i){
            std::uint8_t is_str = std::uint8_t(*p++);
            if (is_str){ std::uint32_t n=detail::get_u32(p);
                for(std::uint32_t j=0;j<n;++j){ std::uint32_t len=detail::get_u32(p); std::string s(p,p+len); p+=len;
                    dict_[i].emplace(s,j); sch_.cats[i].push_back(std::move(s)); } }
        }
        sch_.compute_strides();

        single_shard_ = (sch_.n_shards == 1);
        fdiv_.init(sch_.rows_per_shard);

        const std::string dir = detail::dir_of(meta_path);
        shards_.reserve(sch_.n_shards); data_.reserve(sch_.n_shards);
        for (std::uint32_t i=0;i<sch_.n_shards;++i){
            shards_.push_back(std::make_unique<detail::FileMapRO>(detail::shard_path(dir, sch_.base_name, i)));
            data_.push_back(reinterpret_cast<const double*>(shards_.back()->data()));
        }

        if (warm) prefetch(/*blocking=*/true);
    }

    // Warm the page cache for the whole table (all shards). By default this is
    // never called -- lookups are lazy and only touched pages are paged in.
    //   blocking = false : issue an async readahead hint (MADV_WILLNEED /
    //                      PrefetchVirtualMemory) and return immediately.
    //   blocking = true  : touch every page so the table is fully resident in
    //                      RAM before returning.
    // The return value is a checksum that only prevents the touch loop being
    // optimized away; ignore it. The method is const and thread-safe.
    std::uint64_t prefetch(bool blocking = false) const {
        std::uint64_t s = 0;
        for (const auto& m : shards_) { m->willneed(); if (blocking) s += m->populate(); }
        return s;
    }

    template <typename... K>
    std::uint64_t index(K&&... keys) const {
        std::uint64_t idx = 0; std::size_t d = 0;
        ( (idx += encode_one(d, std::forward<K>(keys)) * sch_.strides[d], ++d), ... );
        return idx;
    }
    template <typename... K>
    const double* at(K&&... keys) const {
        std::uint64_t idx = index(std::forward<K>(keys)...);
        if (single_shard_) return data_[0] + idx*NV;
        std::uint64_t sh = fdiv_.div(idx), loc = idx - sh*sch_.rows_per_shard;
        return data_[sh] + loc*NV;
    }
    std::uint64_t category_index(std::size_t dim, std::string_view s) const { return encode_str(dim, s); }
    const std::string& category_name(std::size_t dim, std::uint64_t i) const { return sch_.cats.at(dim).at(i); }

    struct Span { const double* ptr; std::size_t count; std::size_t stride; };
    template <typename... K>
    Span scan_inner(K&&... outer_keys) const {                 // pass all dims except the last
        std::uint64_t idx = 0; std::size_t d = 0;
        ( (idx += encode_one(d, std::forward<K>(outer_keys)) * sch_.strides[d], ++d), ... );
        std::uint64_t sh = single_shard_ ? 0 : fdiv_.div(idx);
        std::uint64_t loc = idx - sh*sch_.rows_per_shard;
        return Span{ data_[sh] + loc*NV, std::size_t(sch_.dims.back().size), NV };  // whole run lives in one shard
    }

    std::uint64_t rows() const { return sch_.n_rows; }
    const Schema& schema() const { return sch_; }

private:
    template <typename T>
    std::uint64_t encode_one(std::size_t d, const T& k) const {
        if constexpr (std::is_arithmetic_v<std::decay_t<T>>)
            return std::uint64_t(std::int64_t(k) - sch_.dims[d].min);
        else
            return encode_str(d, std::string_view(k));
    }
    std::uint64_t encode_str(std::size_t d, std::string_view s) const {
        auto it = dict_[d].find(std::string(s));
        if (it == dict_[d].end()) throw std::runtime_error("cklut: unknown category '" + std::string(s) + "'");
        return it->second;
    }
    Schema sch_;
    std::vector<std::unordered_map<std::string,std::uint32_t>> dict_;
    std::vector<std::unique_ptr<detail::FileMapRO>> shards_;
    std::vector<const double*> data_;
    detail::FastDivU64 fdiv_;        // fast idx -> shard division
    bool single_shard_ = false;
};

// ===========================================================================
// Low-level Writer (random-access fill by index). Builders sit on top.
// ===========================================================================
class ShardedWriter {
public:
    ShardedWriter(const std::string& out_base, std::vector<DimSpec> specs,
                  std::uint32_t n_values, std::uint64_t max_bytes = 100ull*1024*1024)
        : nv_(n_values) {
        const std::string dir = detail::dir_of(out_base);
        sch_.base_name = dir.empty() ? out_base : out_base.substr(dir.size()+1);

        sch_.dims.resize(specs.size()); sch_.cats.resize(specs.size()); dict_.resize(specs.size());
        for (std::size_t i=0;i<specs.size();++i){
            if (specs[i].is_string()){
                sch_.dims[i] = Dim{0,(std::int64_t)specs[i].categories.size()};
                sch_.cats[i] = specs[i].categories;
                for (std::uint32_t j=0;j<specs[i].categories.size();++j) dict_[i].emplace(specs[i].categories[j], j);
            } else { sch_.dims[i]=Dim{specs[i].min,specs[i].size}; }
        }
        sch_.n_values=nv_;
        sch_.n_rows=1; for(auto&d:sch_.dims) sch_.n_rows*=std::uint64_t(d.size);
        sch_.compute_strides();
        sch_.rows_per_shard = rows_per_shard(sch_.dims, nv_, max_bytes);
        sch_.n_shards = std::uint32_t((sch_.n_rows + sch_.rows_per_shard - 1)/sch_.rows_per_shard);

        const std::uint64_t row_bytes = std::uint64_t(nv_)*sizeof(double);
        for (std::uint32_t i=0;i<sch_.n_shards;++i){
            std::uint64_t rows = std::min<std::uint64_t>(sch_.rows_per_shard, sch_.n_rows - std::uint64_t(i)*sch_.rows_per_shard);
            maps_.push_back(std::make_unique<detail::FileMapRW>(detail::shard_path(dir, sch_.base_name, i), rows*row_bytes));
            data_.push_back(reinterpret_cast<double*>(maps_.back()->data()));
        }
        write_manifest(out_base + ".ckmeta");
    }

    template <typename... K>
    std::uint64_t index(K&&... keys) const {
        std::uint64_t idx=0; std::size_t d=0;
        ( (idx += encode_one(d, std::forward<K>(keys)) * sch_.strides[d], ++d), ... );
        return idx;
    }
    // Encode a categorical key to its dense integer index (used by the builders).
    std::uint64_t index_cat(std::size_t dim, const std::string& s) const {
        auto it = dict_[dim].find(s);
        if (it == dict_[dim].end()) throw std::runtime_error("cklut: unknown category at build");
        return it->second;
    }
    double* row(std::uint64_t idx){
        std::uint64_t sh=idx/sch_.rows_per_shard, loc=idx-sh*sch_.rows_per_shard;
        return data_[sh] + loc*nv_;
    }
    std::uint32_t n_values() const { return nv_; }
    std::uint64_t rows() const { return sch_.n_rows; }
    const Schema& schema() const { return sch_; }

private:
    template <typename T>
    std::uint64_t encode_one(std::size_t d, const T& k) const {
        if constexpr (std::is_arithmetic_v<std::decay_t<T>>) return std::uint64_t(std::int64_t(k)-sch_.dims[d].min);
        else { auto it=dict_[d].find(std::string(std::string_view(k)));
               if(it==dict_[d].end()) throw std::runtime_error("cklut: unknown category at build"); return it->second; }
    }
    void write_manifest(const std::string& path){
        std::vector<char> b; b.insert(b.end(), kMetaMagic, kMetaMagic+8);
        detail::put_u32(b,3); detail::put_u32(b,(std::uint32_t)sch_.dims.size());
        detail::put_u32(b,nv_); detail::put_u32(b,0);
        detail::put_u64(b,sch_.n_rows); detail::put_u64(b,sch_.rows_per_shard);
        detail::put_u32(b,sch_.n_shards);
        detail::put_u32(b,(std::uint32_t)sch_.base_name.size()); b.insert(b.end(), sch_.base_name.begin(), sch_.base_name.end());
        for (auto&d:sch_.dims){ detail::put_u64(b,(std::uint64_t)d.min); detail::put_u64(b,(std::uint64_t)d.size); }
        for (std::size_t i=0;i<sch_.dims.size();++i){
            if (!sch_.cats[i].empty()){ b.push_back(1); detail::put_u32(b,(std::uint32_t)sch_.cats[i].size());
                for(auto&s:sch_.cats[i]){ detail::put_u32(b,(std::uint32_t)s.size()); b.insert(b.end(),s.begin(),s.end()); } }
            else b.push_back(0);
        }
        std::ofstream f(path, std::ios::binary|std::ios::trunc);
        if(!f) throw std::runtime_error("cklut: write meta: "+path);
        f.write(b.data(), (std::streamsize)b.size());
    }

    std::uint32_t nv_;
    Schema sch_;
    std::vector<std::unordered_map<std::string,std::uint32_t>> dict_;
    std::vector<std::unique_ptr<detail::FileMapRW>> maps_;
    std::vector<double*> data_;
};

} // namespace cklut
