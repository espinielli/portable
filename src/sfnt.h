// Minimal sfnt (TTF/OTF/TTC) parser for metrics needed by the PDF writer.
// Only reads table directories and a handful of tables; no glyph outline
// processing.
#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <map>
#include <string>
#include <vector>

namespace sfnt {

struct Metrics {
  bool ok = false;
  bool is_cff = false;
  int units_per_em = 1000;
  int16_t bbox[4] = {0, 0, 1000, 1000};
  int16_t ascender = 800;
  int16_t descender = -200;
  int16_t cap_height = 0;    // 0 if unknown
  double italic_angle = 0.0;
  std::string postscript_name;
  std::vector<uint16_t> advances;   // one entry per glyph, font units
  std::map<uint32_t, uint32_t> cmap;  // glyph id -> unicode codepoint
  std::vector<char> data;           // raw font file bytes to embed
};

inline uint16_t u16(const char* p) {
  return (uint16_t(uint8_t(p[0])) << 8) | uint8_t(p[1]);
}
inline int16_t i16(const char* p) { return int16_t(u16(p)); }
inline uint32_t u32(const char* p) {
  return (uint32_t(uint8_t(p[0])) << 24) | (uint32_t(uint8_t(p[1])) << 16) |
         (uint32_t(uint8_t(p[2])) << 8) | uint32_t(uint8_t(p[3]));
}

struct Table {
  uint32_t offset = 0;
  uint32_t length = 0;
  bool found = false;
};

// Parse a cmap format 4 subtable into codepoint -> glyph mappings
inline void cmap_format4(const char* p, uint32_t length,
                         std::map<uint32_t, uint32_t>& cp_to_gid) {
  uint16_t seg_count = u16(p + 6) / 2;
  const char* end_codes = p + 14;
  const char* start_codes = end_codes + 2 * seg_count + 2;
  const char* id_deltas = start_codes + 2 * seg_count;
  const char* id_range_off = id_deltas + 2 * seg_count;
  for (uint16_t s = 0; s < seg_count; s++) {
    uint16_t start = u16(start_codes + 2 * s);
    uint16_t end = u16(end_codes + 2 * s);
    int16_t delta = i16(id_deltas + 2 * s);
    uint16_t range_off = u16(id_range_off + 2 * s);
    for (uint32_t cp = start; cp <= end && cp != 0xFFFF; cp++) {
      uint32_t gid;
      if (range_off == 0) {
        gid = (cp + delta) & 0xFFFF;
      } else {
        const char* g = id_range_off + 2 * s + range_off + 2 * (cp - start);
        if (g + 2 > p + length) break;
        gid = u16(g);
        if (gid != 0) gid = (gid + delta) & 0xFFFF;
      }
      if (gid != 0) cp_to_gid[cp] = gid;
    }
  }
}

// Parse a cmap format 12 subtable into codepoint -> glyph mappings
inline void cmap_format12(const char* p, uint32_t length,
                          std::map<uint32_t, uint32_t>& cp_to_gid) {
  uint32_t n_groups = u32(p + 12);
  for (uint32_t g = 0; g < n_groups; g++) {
    const char* grp = p + 16 + g * 12;
    if (grp + 12 > p + length) break;
    uint32_t start_cp = u32(grp);
    uint32_t end_cp = u32(grp + 4);
    uint32_t start_gid = u32(grp + 8);
    if (end_cp - start_cp > 0x100000) break;  // sanity
    for (uint32_t cp = start_cp; cp <= end_cp; cp++) {
      cp_to_gid[cp] = start_gid + (cp - start_cp);
    }
  }
}

inline Table find_table(const char* data, size_t size, uint32_t dir_off,
                        const char* tag) {
  Table t;
  if (dir_off + 4 > size) return t;
  uint16_t n_tables = u16(data + dir_off + 4);
  for (uint16_t i = 0; i < n_tables; i++) {
    size_t rec = dir_off + 12 + size_t(i) * 16;
    if (rec + 16 > size) break;
    if (std::memcmp(data + rec, tag, 4) == 0) {
      t.offset = u32(data + rec + 8);
      t.length = u32(data + rec + 12);
      t.found = t.offset + t.length <= size;
      return t;
    }
  }
  return t;
}

inline bool parse(const std::string& path, unsigned int index, Metrics& out) {
  std::ifstream in(path, std::ios::binary);
  if (!in) return false;
  in.seekg(0, std::ios::end);
  size_t size = in.tellg();
  in.seekg(0, std::ios::beg);
  out.data.resize(size);
  if (!in.read(out.data.data(), size)) return false;

  const char* d = out.data.data();
  if (size < 12) return false;

  uint32_t dir_off = 0;
  bool is_cff = false;
  if (std::memcmp(d, "ttcf", 4) == 0) {
    // TrueType/OpenType collection: pick the face at `index`
    uint32_t n_fonts = u32(d + 8);
    if (index >= n_fonts) return false;
    if (12 + (size_t(index) + 1) * 4 > size) return false;
    dir_off = u32(d + 12 + size_t(index) * 4);
    if (dir_off + 4 > size) return false;
  }
  uint32_t version = u32(d + dir_off);
  is_cff = version == 0x4F54544F;  // 'OTTO'
  if (!is_cff && version != 0x00010000 && version != 0x74727565 /* 'true' */) {
    return false;
  }

  Table head = find_table(d, size, dir_off, "head");
  Table hhea = find_table(d, size, dir_off, "hhea");
  Table maxp = find_table(d, size, dir_off, "maxp");
  Table post = find_table(d, size, dir_off, "post");
  if (!head.found || !hhea.found || !maxp.found) return false;

  out.is_cff = is_cff;
  out.units_per_em = u16(d + head.offset + 18);
  if (out.units_per_em <= 0) out.units_per_em = 1000;
  for (int i = 0; i < 4; i++) out.bbox[i] = i16(d + head.offset + 36 + i * 2);
  out.ascender = i16(d + hhea.offset + 4);
  out.descender = i16(d + hhea.offset + 6);
  uint16_t n_metrics = u16(d + hhea.offset + 34);
  uint16_t n_glyphs = u16(d + maxp.offset + 4);
  if (post.found && post.length >= 6) {
    out.italic_angle = double(int32_t(u32(d + post.offset + 4))) / 65536.0;
  }

  Table os2 = find_table(d, size, dir_off, "OS/2");
  if (os2.found && os2.length >= 90 && u16(d + os2.offset) >= 2) {
    out.cap_height = i16(d + os2.offset + 88);
  }

  // PostScript name: name record with ID 6, preferring Windows (UTF-16BE)
  Table name = find_table(d, size, dir_off, "name");
  if (name.found && name.length >= 6) {
    uint16_t count = u16(d + name.offset + 2);
    uint16_t str_off = u16(d + name.offset + 4);
    std::string fallback;
    for (uint16_t i = 0; i < count; i++) {
      size_t rec = name.offset + 6 + size_t(i) * 12;
      if (rec + 12 > size) break;
      uint16_t platform = u16(d + rec);
      uint16_t name_id = u16(d + rec + 6);
      uint16_t len = u16(d + rec + 8);
      uint16_t off = u16(d + rec + 10);
      if (name_id != 6) continue;
      size_t s = name.offset + str_off + off;
      if (s + len > size) continue;
      if (platform == 3) {
        std::string ps;
        for (uint16_t j = 0; j + 1 < len; j += 2) {
          char c = d[s + j + 1];
          ps += (c >= 33 && c <= 126 && c != ' ') ? c : '-';
        }
        out.postscript_name = ps;
        break;
      } else if (fallback.empty()) {
        fallback.assign(d + s, len);
      }
    }
    if (out.postscript_name.empty()) out.postscript_name = fallback;
  }

  // Horizontal advances from hmtx
  Table hmtx = find_table(d, size, dir_off, "hmtx");
  if (hmtx.found && n_metrics > 0 && n_glyphs > 0) {
    out.advances.resize(n_glyphs);
    uint16_t last = 0;
    for (uint32_t i = 0; i < n_glyphs; i++) {
      uint32_t g = i < n_metrics ? i : n_metrics - 1;
      if (hmtx.offset + g * 4 + 2 <= size) {
        last = u16(d + hmtx.offset + g * 4);
      }
      out.advances[i] = last;
    }
  }

  // Character map: build glyph -> codepoint from the best cmap subtable
  Table cmap = find_table(d, size, dir_off, "cmap");
  if (cmap.found && cmap.length >= 4) {
    const char* ct = d + cmap.offset;
    uint16_t n_sub = u16(ct + 2);
    int best4 = -1, best12 = -1;
    for (uint16_t i = 0; i < n_sub; i++) {
      const char* rec = ct + 4 + i * 8;
      if (rec + 8 > d + size) break;
      uint16_t platform = u16(rec);
      uint32_t sub_off = u32(rec + 4);
      if (sub_off + 2 > cmap.length) continue;
      uint16_t format = u16(ct + sub_off);
      if (format == 12 && (platform == 3 || platform == 0)) best12 = (int)sub_off;
      if (format == 4 && (platform == 3 || platform == 0)) best4 = (int)sub_off;
    }
    std::map<uint32_t, uint32_t> cp_to_gid;
    if (best12 >= 0) {
      const char* p = ct + best12;
      cmap_format12(p, u32(p + 4), cp_to_gid);
    } else if (best4 >= 0) {
      const char* p = ct + best4;
      cmap_format4(p, u16(p + 2), cp_to_gid);
    }
    for (auto& kv : cp_to_gid) out.cmap[kv.second] = kv.first;
  }

  out.ok = true;
  // The raw bytes are only needed during parsing; the embedded font is
  // synthesized from outlines (see build_ttf) so drop them
  out.data.clear();
  out.data.shrink_to_fit();
  return true;
}

// ---------------------------------------------------------------------------
// TrueType subset synthesis, ported from the hephaestus crate's
// src/backend/pdf/sfnt.rs. A new font is built from extracted outlines
// rather than sliced out of the source face, so one code path covers
// TrueType, CFF, variable fonts, and collections. The file carries six
// tables: glyf, head, hhea, hmtx, loca, maxp.
// ---------------------------------------------------------------------------

struct GlyfPoint {
  int16_t x, y;
  bool on_curve;
};

struct GlyfGlyph {
  std::vector<std::vector<GlyfPoint>> contours;

  bool is_empty() const {
    for (auto& c : contours)
      if (!c.empty()) return false;
    return true;
  }
  void bbox(int16_t* b) const {
    b[0] = b[1] = INT16_MAX;
    b[2] = b[3] = INT16_MIN;
    bool any = false;
    for (auto& c : contours) {
      for (auto& p : c) {
        b[0] = std::min(b[0], p.x);
        b[1] = std::min(b[1], p.y);
        b[2] = std::max(b[2], p.x);
        b[3] = std::max(b[3], p.y);
        any = true;
      }
    }
    if (!any) b[0] = b[1] = b[2] = b[3] = 0;
  }
  size_t point_count() const {
    size_t n = 0;
    for (auto& c : contours) n += c.size();
    return n;
  }
};

// Approximate a cubic with a single midpoint-matched quadratic, or split
// and recurse when the cubic is not flat enough
inline void flatten_cubic(double x0, double y0, double x1, double y1,
                          double x2, double y2, double x3, double y3,
                          double tol, std::vector<GlyfPoint>& contour,
                          int depth) {
  // Flatness: max distance of the control points from the chord
  double cx = x3 - x0, cy = y3 - y0;
  double len = std::hypot(cx, cy);
  double d1 = 0, d2 = 0;
  if (len > 0) {
    d1 = std::fabs((x1 - x0) * cy - (y1 - y0) * cx) / len;
    d2 = std::fabs((x2 - x0) * cy - (y2 - y0) * cx) / len;
  } else {
    d1 = std::hypot(x1 - x0, y1 - y0);
    d2 = std::hypot(x2 - x0, y2 - y0);
  }
  if (std::max(d1, d2) <= tol || depth >= 24) {
    // Control point so the quad matches the cubic at t = 0.5
    double qx = (3.0 * (x1 + x2) - x0 - x3) / 4.0;
    double qy = (3.0 * (y1 + y2) - y0 - y3) / 4.0;
    contour.push_back({(int16_t)std::lround(qx), (int16_t)std::lround(qy), false});
    contour.push_back({(int16_t)std::lround(x3), (int16_t)std::lround(y3), true});
    return;
  }
  // de Casteljau split at t = 0.5
  double xa = (x0 + x1) / 2, ya = (y0 + y1) / 2;
  double xb = (x1 + x2) / 2, yb = (y1 + y2) / 2;
  double xc = (x2 + x3) / 2, yc = (y2 + y3) / 2;
  double xd = (xa + xb) / 2, yd = (ya + yb) / 2;
  double xe = (xb + xc) / 2, ye = (yb + yc) / 2;
  double xf = (xd + xe) / 2, yf = (yd + ye) / 2;
  flatten_cubic(x0, y0, xa, ya, xd, yd, xf, yf, tol, contour, depth + 1);
  flatten_cubic(xf, yf, xe, ye, xc, yc, x3, y3, tol, contour, depth + 1);
}

// Parse the path format produced by systemfonts' get_glyph_path()
// ("M x y L x y Q cx cy x y C c1x c1y c2x c2y x y Z ..."). Coordinates
// are font units when the outline was extracted at size == upem.
inline GlyfGlyph parse_glyph_path(const std::string& path, double tol) {
  GlyfGlyph glyph;
  std::vector<GlyfPoint> open;
  auto flush = [&]() {
    if (!open.empty()) {
      glyph.contours.push_back(open);
      open.clear();
    }
  };

  const char* p = path.c_str();
  char cmd = 0;
  double coords[6];
  int n_coord = 0;
  double cur_x = 0, cur_y = 0;
  while (*p) {
    if (std::isalpha(*p)) {
      cmd = *p++;
      if (cmd == 'Z') {
        flush();
        n_coord = 0;
      }
      continue;
    }
    if (*p == ' ' || *p == '\n') {
      p++;
      continue;
    }
    char* end;
    double v = std::strtod(p, &end);
    if (end == p) {
      p++;
      continue;
    }
    p = end;
    coords[n_coord++] = v;
    auto push = [&](double x, double y, bool on) {
      open.push_back({(int16_t)std::lround(x), (int16_t)std::lround(y), on});
      cur_x = x;
      cur_y = y;
    };
    if (cmd == 'M' && n_coord == 2) {
      flush();
      push(coords[0], coords[1], true);
      n_coord = 0;
    } else if (cmd == 'L' && n_coord == 2) {
      push(coords[0], coords[1], true);
      n_coord = 0;
    } else if (cmd == 'Q' && n_coord == 4) {
      push(coords[0], coords[1], false);
      push(coords[2], coords[3], true);
      n_coord = 0;
    } else if (cmd == 'C' && n_coord == 6) {
      flatten_cubic(cur_x, cur_y, coords[0], coords[1], coords[2], coords[3],
                    coords[4], coords[5], tol, open, 0);
      cur_x = coords[4];
      cur_y = coords[5];
      n_coord = 0;
    }
  }
  flush();
  return glyph;
}

inline void be16(std::vector<char>& v, int16_t x) {
  v.push_back((char)(x >> 8));
  v.push_back((char)(x & 0xFF));
}
inline void be16u(std::vector<char>& v, uint16_t x) { be16(v, (int16_t)x); }
inline void be32(std::vector<char>& v, uint32_t x) {
  v.push_back((char)(x >> 24));
  v.push_back((char)((x >> 16) & 0xFF));
  v.push_back((char)((x >> 8) & 0xFF));
  v.push_back((char)(x & 0xFF));
}

inline uint32_t table_checksum(const std::vector<char>& data) {
  uint32_t sum = 0;
  size_t i = 0;
  for (; i + 4 <= data.size(); i += 4) {
    sum += (uint32_t(uint8_t(data[i])) << 24) |
           (uint32_t(uint8_t(data[i + 1])) << 16) |
           (uint32_t(uint8_t(data[i + 2])) << 8) | uint8_t(data[i + 3]);
  }
  if (i < data.size()) {
    uint32_t word = 0;
    for (size_t j = 0; i + j < data.size(); j++) {
      word |= uint32_t(uint8_t(data[i + j])) << (24 - 8 * j);
    }
    sum += word;
  }
  return sum;
}

// One coordinate axis as glyf deltas plus the flags describing the encoding
inline void deltas(const std::vector<int16_t>& values, uint8_t short_bit,
                   uint8_t same_bit, std::vector<char>& bytes,
                   std::vector<uint8_t>& flags) {
  int32_t prev = 0;
  for (int16_t v : values) {
    int32_t d = int32_t(v) - prev;
    prev = v;
    if (d == 0) {
      flags.push_back(same_bit);
    } else if (d > -256 && d < 256) {
      flags.push_back(short_bit | (d > 0 ? same_bit : 0));
      bytes.push_back((char)(d < 0 ? -d : d));
    } else {
      flags.push_back(0);
      be16(bytes, (int16_t)d);
    }
  }
}

inline void write_simple_glyph(std::vector<char>& out, const GlyfGlyph& g,
                               const int16_t* bbox) {
  std::vector<const std::vector<GlyfPoint>*> contours;
  for (auto& c : g.contours)
    if (!c.empty()) contours.push_back(&c);
  be16(out, (int16_t)contours.size());
  for (int i = 0; i < 4; i++) be16(out, bbox[i]);
  int32_t end = 0;
  for (auto* c : contours) {
    end += (int32_t)c->size();
    be16u(out, (uint16_t)(end - 1));
  }
  be16u(out, 0);  // instructionLength

  size_t n = g.point_count();
  std::vector<int16_t> xv, yv;
  std::vector<uint8_t> on;
  xv.reserve(n);
  yv.reserve(n);
  on.reserve(n);
  for (auto* c : contours) {
    for (auto& p : *c) {
      xv.push_back(p.x);
      yv.push_back(p.y);
      on.push_back(p.on_curve ? 1 : 0);
    }
  }
  std::vector<char> xs, ys;
  std::vector<uint8_t> xf, yf;
  deltas(xv, 0x02, 0x10, xs, xf);
  deltas(yv, 0x04, 0x20, ys, yf);
  for (size_t i = 0; i < n; i++) {
    out.push_back((char)(on[i] | xf[i] | yf[i]));
  }
  out.insert(out.end(), xs.begin(), xs.end());
  out.insert(out.end(), ys.begin(), ys.end());
}

// Assemble a TrueType file carrying exactly `glyphs`; glyphs[0] should be
// an empty .notdef. Ported from hephaestus sfnt::build.
inline std::vector<char> build_ttf(const std::vector<GlyfGlyph>& glyphs,
                                   const std::vector<uint16_t>& advances,
                                   uint16_t upem, int16_t ascent,
                                   int16_t descent, int16_t line_gap) {
  size_t n = glyphs.size();
  std::vector<char> glyf, loca_bytes;
  std::vector<uint32_t> loca;
  int16_t bbox[4] = {INT16_MAX, INT16_MAX, INT16_MIN, INT16_MIN};
  std::vector<int16_t> lsbs;
  size_t max_points = 0, max_contours = 0;
  for (auto& g : glyphs) {
    loca.push_back((uint32_t)glyf.size());
    int16_t b[4];
    g.bbox(b);
    lsbs.push_back(b[0]);
    if (!g.is_empty()) {
      for (int i = 0; i < 4; i++) {
        bbox[i] = i < 2 ? std::min(bbox[i], b[i]) : std::max(bbox[i], b[i]);
      }
      write_simple_glyph(glyf, g, b);
      while (glyf.size() % 4 != 0) glyf.push_back(0);
    }
    max_points = std::max(max_points, g.point_count());
    max_contours = std::max(max_contours, g.contours.size());
  }
  loca.push_back((uint32_t)glyf.size());
  if (bbox[0] == INT16_MAX) bbox[0] = bbox[1] = bbox[2] = bbox[3] = 0;
  for (uint32_t o : loca) be32(loca_bytes, o);

  std::vector<char> head;
  be32(head, 0x00010000);
  be32(head, 0x00010000);
  be32(head, 0);  // checkSumAdjustment, patched last
  be32(head, 0x5F0F3CF5);
  be16u(head, 3);
  be16u(head, upem);
  be32(head, 0);
  be32(head, 0);  // created
  be32(head, 0);
  be32(head, 0);  // modified
  for (int i = 0; i < 4; i++) be16(head, bbox[i]);
  be16u(head, 0);  // macStyle
  be16u(head, 8);  // lowestRecPPEM
  be16(head, 2);   // fontDirectionHint
  be16(head, 1);   // indexToLocFormat: long
  be16(head, 0);   // glyphDataFormat

  uint16_t max_adv = 0;
  int16_t min_lsb = 0;
  for (uint16_t a : advances) max_adv = std::max(max_adv, a);
  for (int16_t l : lsbs) min_lsb = std::min(min_lsb, l);
  std::vector<char> hhea;
  be32(hhea, 0x00010000);
  be16(hhea, ascent);
  be16(hhea, descent);
  be16(hhea, line_gap);
  be16u(hhea, max_adv);
  be16(hhea, min_lsb);
  be16(hhea, 0);        // minRightSideBearing
  be16(hhea, bbox[2]);  // xMaxExtent
  be16(hhea, 1);        // caretSlopeRise
  be16(hhea, 0);        // caretSlopeRun
  be16(hhea, 0);        // caretOffset
  for (int i = 0; i < 4; i++) be16(hhea, 0);
  be16(hhea, 0);  // metricDataFormat
  be16u(hhea, (uint16_t)n);

  std::vector<char> hmtx;
  for (size_t i = 0; i < n; i++) {
    be16u(hmtx, advances[i]);
    be16(hmtx, lsbs[i]);
  }

  std::vector<char> maxp;
  be32(maxp, 0x00010000);
  be16u(maxp, (uint16_t)n);
  be16u(maxp, (uint16_t)std::min(max_points, size_t(0xFFFF)));
  be16u(maxp, (uint16_t)std::min(max_contours, size_t(0xFFFF)));
  be16u(maxp, 0);  // maxCompositePoints
  be16u(maxp, 0);  // maxCompositeContours
  be16u(maxp, 2);  // maxZones
  for (int i = 0; i < 7; i++) be16u(maxp, 0);
  be16u(maxp, 0);  // maxComponentDepth

  // Table directory (sorted by tag) and final assembly
  const char* tags[6] = {"glyf", "head", "hhea", "hmtx", "loca", "maxp"};
  std::vector<char>* tables[6] = {&glyf, &head, &hhea, &hmtx, &loca_bytes, &maxp};
  uint16_t count = 6, entry_selector = 2;  // floor(log2(6))
  uint16_t search_range = 16 << entry_selector;
  uint16_t range_shift = count * 16 - search_range;

  std::vector<char> out;
  be32(out, 0x00010000);
  be16u(out, count);
  be16u(out, search_range);
  be16u(out, entry_selector);
  be16u(out, range_shift);
  size_t directory_start = out.size();
  out.resize(directory_start + count * 16, 0);
  size_t head_offset = 0;
  for (int i = 0; i < 6; i++) {
    while (out.size() % 4 != 0) out.push_back(0);
    size_t offset = out.size();
    if (std::memcmp(tags[i], "head", 4) == 0) head_offset = offset;
    size_t rec = directory_start + i * 16;
    std::memcpy(&out[rec], tags[i], 4);
    uint32_t cs = table_checksum(*tables[i]);
    uint32_t off32 = (uint32_t)offset, len32 = (uint32_t)tables[i]->size();
    std::vector<char> tmp;
    be32(tmp, cs);
    be32(tmp, off32);
    be32(tmp, len32);
    std::memcpy(&out[rec + 4], tmp.data(), 12);
    out.insert(out.end(), tables[i]->begin(), tables[i]->end());
  }
  while (out.size() % 4 != 0) out.push_back(0);
  // head's own checksum was taken with this field zero, as required
  uint32_t adjustment = 0xB1B0AFBAu - table_checksum(out);
  out[head_offset + 8] = (char)(adjustment >> 24);
  out[head_offset + 9] = (char)((adjustment >> 16) & 0xFF);
  out[head_offset + 10] = (char)((adjustment >> 8) & 0xFF);
  out[head_offset + 11] = (char)(adjustment & 0xFF);
  return out;
}

}  // namespace sfnt
