# 绿色指标统计算法说明

## 1.标准分

### 来源
外部计算

### 算法
暂不详

### 应用
- 总分
- 题组合计得分


## 2.等级

### 来源
外部计算

### 算法
暂不详

### 应用
- 总分
- 题组合计得分

## 3.权重

### 来源
外部计算（考务抽样系统）

### 算法：

1. 按测试地区进行权重计算
	
	1.1 各个地区学校抽样方法不一样
	
	1.2 各个地区学生抽样方法不一样2. 计算前数据检测	2.1	 根据核实信息，对第一次上报的的学校信息进行处理（根据变更文档进行处理）	2.2 在抽样的学校中是主校，而在基础库中是主校+分校，需要修改基础库，把分校的学生合并到主校中	2.3 检测各个学科的实际测试学生能否在基础库中能匹配上，如果不能匹配，表示基础库缺少学生，需要在增加缺少的学生的各个信息	2.4 检测基础库中的学校是否参加了相应地区年级的考试，如果未参加，需要从基础库中删除该学校的学生信息	2.5 检测基础库中抽样的学校是否在第一次上报的学校库中存在，如果不存在，需要在上报的学校库中增加相应的学校信息，年级总人数和学校变量人数从基础库中进行统计。	2.6 检测基础库中的学校是否在学校对照表中存在，如果不存在，则需要在学校对照表中增加相应的学校对照信息	2.7 检测第二次上报的学生数据中的学校代码是否在第一次上报的学校数据中存在，如果不存在，需要查原因，进行相应信息的更正		2.7.1 前后两次的学校代码不一致，需要修改一致		2.7.2 抽样学校变更等等原因3. 更新学校年级人数和学生抽样变量人数	原因：第一次上报的学校数据中的学校年级人数及抽样变量人数不准确，需要从第二次上报的学生数据中重新统计出正确的数据。	3.1 从第二次上报的学生数据更新第一次上报的学校年级人数	3.2 如果地区有学生抽样变量，需要从第二次上报的学生数据更新第一次上报的学生抽样变量人数4. 从学科测试数据中统计出实际数据	4.1 每个学科只有一个权重数据，		4.1.1 如果学科只有一个试卷，则直接可用该试卷清理后的数据进行统计		4.1.2 如果学科有多个试卷，则需要在各个试卷清理完毕后合并学生准考证号到一个表中后进行统计	4.2 学生问卷只有一个权重数据，以学生问卷公共问卷清理后的数据进行统计	4.3 统计方法		4.3.1 无学生抽样变量，统计出各个学校的学科学生实际测试人数		4.3.2 有学生抽样变量，统计出各个学校各个学生抽样变量的学生实际测试人数5. 如果有学校变量，根据实际变量统计数据生成新的学校年级人数和学生抽样变量人数	5.1 规则:如果学校变量无实际测试人数（可能被清理掉），其他最大变量人数=其他最大变量人数+该变量人数，该变量人数清06. 统计区县类型统计数据	6.1 不同的学科参测的学校可能不一样，因此，每个学科需要建立自己学科的区县统计数据	6.2 根据实测学校的学校变量信息，统计区县类型统计数据（即：剔除区县未参加测试的学校变量的学校后进行统计）	6.3 对全测+抽样的区县，需要将全测的区县剔除后进行统计7. 计算权重	7.1 统计区县总人数		7.1.1 抽样：从区县类型统计数据中统计出各区县的总人数		7.1.2 全测+抽样：从区县类型统计数据中（需要剔除掉全测的学校）统计出各区县的总人数	7.2 统计区县实际参加测试学生数		7.2.1 抽样：从各校实际参测人数中统计出各区县的总实际人数		7.2.2 全测+抽样：从各校实际参测人数中（需要剔除掉全测的学校）统计出各区县的总实际人数	7.3 计算FW1		7.3.1 FW1=区县类型抽样总人数/(区县抽中类型抽样学校个数*学校总人数)	7.4 计算FW2		7.4.1 FW2=学校类型人数/学校实际参测人数	7.5 计算FW		7.5.1 FW=FW1*FW2		7.5.2 全测的学校FW=1		7.5.3 全测的实际测试学校未在第一次上报的学校中的处理方法：			7.5.3.1 在权重结果表中增加未报的全测学校			7.5.3.2 FW=1			7.5.3.3 学校实际参测人数从各校实际参测人数表中取	7.6 统计地区总人数		7.6.1 抽样：从区县类型统计数据中统计出地区总人数		7.6.2 全测+抽样：从区县类型统计数据中统计出地区抽样总人数+全测试总人数	7.7 统计学科测试实际总人数		7.7.1 从各校实际参测人数中统计出地区的总实际人数	7.8 计算地区W		7.8.1 W=FW*学科测试实际总人数/地区总人数	7.9 计算地区的区县QXW		7.9.1 QXW=FW*区县实际参加测试学生数/区县总人数8. 权限验证	8.1 地区总体验证		8.1.1 地区总人数回归：FW*学校实际参测人数 地区学校之和 是否与地区总人数相等（精确到小数点后10位即可）		8.1.2 地区总实际人数回归：W*学校实际参测人数 地区学校之和 是否与地区总实际人数相等（精确到小数点后10位即可）	8.2 区县验证		8.2.1 区县总人数回归：FW*学校实际参测人数 各区县学校之和 是否与区县总人数相等（精确到小数点后10位即可）		8.2.2 区县总实际人数回归：qxW*学校实际参测人数 各区县学校之和 是否与区县总实际人数相等（精确到小数点后10位即可）### 应用
- 学科成绩百分比
- 学科等级百分比
- 学生问卷答案百分比
- 家长问卷答案百分比


## 4.总分百分位

### 来源
统计计算

### 算法
- 按学科成绩总分，对成绩记录进行百分位计算

### 应用
- 区县／学校的学科总分分布盒须图
	- 95分位
	- 75分位
	- 50分位
	- 25分位
	- 5分位 

## 学业成绩

### 来源
统计计算

### 算法
- 语文，数学，英语，科学四科平均分的的再平均
	- 学业成绩 = mean( mean(语文)，mean(数学)，mean(英语)，mean(科学))

### 应用
- 区县／学校学业成绩与变量／指标关系散点图

## 学业标准达成指数

### 来源
统计计算

### 算法
- 不及格率：percentile(x, total_level = 'D')
- 公式：i = 1 - 2.5 * percentile(x, total_level = 'D')

### 应用
- 市，区，校学科学业标准达成指数

## 高层次思维能力发展指数

### 来源
暂不详

### 算法
暂不详

### 应用
- 市，区，校学生高层次思维能力发展指数


## 学业成绩均衡指数

### 来源
统计计算

### 算法
- 个体间均衡指数：
	- 公式：i = 1 - 2.5 * ( sd(total_score) / mean(total_score) )
- 学校间均衡指数：
	- 公式：i = 1 - 2.5 * （ School／(School + Student) )
	- 参数：
		- School：
			- 来源：外部计算
			- 算法：暂不详 
		- Student：	
			- 来源：外部计算
			- 算法：暂不详 	
- 区县间均衡指数
 	- 公式：i = 1 - 2.5 * （ ZoneRate／(ZoneRate + ZoneStudent) )
	- 参数：
		- ZoneRate：
			- 来源：外部计算
			- 算法：暂不详 
		- ZoneStudent：	
			- 来源：外部计算
			- 算法：暂不详 	

## *社会经济背景与学业成绩相关指数（家庭背景）*

### 来源
统计计算

### 算法
- 公式：i = 2.5 * (School - Center) / (School + Student) - 0.125
- 参数：
	- School：
		- 来源：外部计算
		- 算法：暂不详 
	- Student：	
		- 来源：外部计算
		- 算法：暂不详 	
	- Center：
		- 来源：外部计算
		- 算法：暂不详 


## 问卷调查综合指数

### 来源
统计计算

### 算法
1. 单项答题结果量化：
	- 公式：topic_score = select(a, b, c) ? 1 : 0
	- 原则：二值量化的0/1选项依照每个问题独立定义，通常正向选择为1，负向选择为0，中性选项缺省为0
	- 调整：实际根据初步统计分析结果可能调整二值量化选项值 
2. 题组得分计算：
	- 公式：group_score = sum( topic_score )
3. 综合指数计算：
	- 达标公式：var = group_score/count(topics) > 60% ? TRUE : FALSE
	- 指标公式：i = floor( percentile(var =TRUE) )

### 应用
- 学生，家长，教师，校长问卷的综合指数

### 百分比验证通过
- 睡眠7小时以上 45.29 vs 45.31 
- 作业3小时以上 41.18 vs 41.16
- 每周补课6小时以上 19.20 vs 19.19
- 


### 百分比验证未通过
- 师生关系较好 83.4 vs 77.6
- 学习压力较小 66.9 vs 46.5
- 学校认同度 89.6 vs 80.6
- 学习动机 80.1 vs 67.9
- 自信心较强 75.1 vs 67.7


### 指数验证通过
- 校长课程领导力指数 78.9 vs i7
- 教师教学方式指数 66.6 vs i6
- 校内补课指数 62.0 vs i6 (351/566)
- 作业指数 57.8 vs i5
- 学校认同度指数  89.6 vs i8（百分比验证不符 89.6 vs 80.6）

### 指数验证未通过
- 学生品德行为指数 81.7 vs i7
- 师生关系指数 83.4 vs i7
- 校外补课指数
- 睡眠指数
- 学业负担指数
- 学习压力指数
- 学习动力指数
- 学习自信心指数

### 指数尚未验证

### 指数算法尚不明


## 体质健康指数

### 来源
外部计算（体质监测系统）

### 算法
不详

### 应用
- 学生健康指数
- 仅提供区县指数