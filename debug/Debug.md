
# 常见问题调试记录

1. Error in unname(colors[i][seq_len(n)]) : object 'i' not found
plot_fill_order的列表与plot_fill实际的数据不吻合

2. Removed n rows containing missing values (geom_bar).
plot_limit_y 小于数据展示值域，例如0┋100，需改为0┋101
