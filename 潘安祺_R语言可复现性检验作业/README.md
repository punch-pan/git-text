# R语言可复现性检验作业

**作者**: 潘安祺  
**课程**: 心理学院 R 编程语言课程  
**截止日期**: 2026年6月30日  

---

## 📋 提交内容清单

| 序号 | 内容 | 路径 | 说明 |
|------|------|------|------|
| 1 | 可复现性报告 | `对 Christensen et al. (2018) 研究结果的计算可复现性检验报告.docx` | Word 格式完整报告 |
| 2 | 汇报 PPT | `report/report_standalone.html` | 自包含 HTML 幻灯片（内含所有图片，离线可打开） |
| 3 | 复现代码 | `Replication_Code.R` | R 脚本，包含完整复现流程 |
| 4 | 复现图片 | `figures/` 文件夹 | 5 张复现生成的图表 |
| 5 | 数据表格 | `table2-7/` 文件夹 | 6 个结果对比 CSV 表格 |
| 6 | 原始 HTML | `report/report.html` / `report/report.Rhtml` | R 生成的原始汇报文件 |

---

## 🔗 研究对象

- **原文**: Christensen et al. (2018), *European Journal of Personality*
- **主题**: 开放性人格与语义记忆网络结构
- **数据**: OSF 平台公开数据（osf.io/craky/）
- **核心假设**: H1（ASPL 更低）、H2（CC 更高）、H3（Q 更低）

---

## 🚀 快速复现

```r
# 在 R 中运行
source("Replication_Code.R")
```

---

## 📁 项目结构

```
.
├── Replication_Code.R                           # 复现 R 代码
├── 对 Christensen et al. (2018) 研究结果的计算可复现性检验报告.docx  # 完整报告
├── README.md                                    # 本说明文件
├── figures/                                     # 复现图片
│   ├── combined_figure.png
│   ├── consistency_plot.png
│   ├── descriptive_plot.png
│   ├── pe_plot.png
│   └── reproducibility_donut.png
├── report/                                      # 汇报文件
│   ├── report.html                              # 原始 HTML（需 figures 配合）
│   ├── report.Rhtml                             # R HTML 源文件
│   └── report_standalone.html                   # 自包含汇报（离线可用）
└── table2-7/                                    # 数据表格
    ├── table2_desc_compare.csv                  # 描述性统计对比
    ├── table3_infer_compare.csv                 # 推断统计对比
    ├── table4_new_method.csv                    # 创新方法（MST）结果
    ├── table5_reproducibility.csv               # 可复现性评级
    ├── table6_consistency_old.csv               # 原方法推论一致性
    └── table7_consistency_new.csv             # 创新方法推论一致性
```

---

## 💡 说明

- `report/report_standalone.html` 为**自包含文件**，所有图片和附件已用 Base64 编码内嵌，无需 `figures` 文件夹即可在任何浏览器中完整显示图片和下载附件。
- `report/report.html` 为原始 HTML 文件，需与 `figures/` 文件夹保持在同一目录下才能正常显示图片。

