# Remusys Compiler

CSCC 2025 参赛项目, Remusys 编译器的驱动器, 主管输入输出、代码生成、流程管理等。

其余部分参见:

- 前端: [Remusys Lang](https://github.com/medihbt/remusys-lang) 使用 [LALRPOP](https://lalrpop.github.io/lalrpop) 编写的前端, 
- 中间代码、优化器、后端: [Remusys IR](https://github.com/medihbt/remusys-ir)
- 后端生成器: [Remusys InstGen](https://codeberg.org/medihbt/remusys-instgen)

## ⚠️ 警告

**该项目是实验性项目, 仅可用于学习研究, 请勿用于实际生产环境!**

**项目没有任何 API 稳定性, 代码架构、接口、实现等随时发生破坏性变化, 倘若造成后果, 则由使用者自负.**

**项目可能随时停止开发或者转为归档状态**.

## 构建指南

首先克隆该项目:

```bash
git clone https://github.com/medihbt/remusys-compiler
git submodule update --init --recursive

# 若子项目没 clone 到本地, 则执行
pushd remusys-ir && git checkout master && git pull -r && popd
pushd remusys-lang && git checkout master && git pull -r && popd
```

然后 `cargo build` 即可完成构建—— LALRPOP 和 Rig 生成的内容已经预生成完毕了, `cargo build` 会直接使用这里的缓存.

### 如果想更新子项目生成的文件?

对于前端, 你需要额外安装 lalrpop:

```bash
# 安装 lalrpop 可执行文件
cargo install lalrpop
# 更新语法文件后得重新做生成: cd 到 remusys-lang 目录然后执行:
resource/regenerate_parser.sh
```

对于后端, 你需要额外下载并编译 `remusys-instgen` 项目:

```bash
# 下载并编译 Rig 编译器
git clone https://codeberg.org/medihbt/remusys-instgen && cd remusys-instgen
cargo build

# 编辑 Rig 后重新生成所有相关文件:
data/run-rig.sh
```

**如果想自定义操作数?** ... 那很麻烦了, 因为 Remusys MIR 操作数定义是用 Rust 编写的. 要新增操作数, 你需要修改这么几个东西：

- `remusys_ir::mir::operand` 模块里的 `MirOperand` 与相关枚举, 自定义操作数类型, 自己实现 `IMirSubOperand` trait.
- `remusys-instgen` 里的操作数关键字定义, 不止一处文件要更改哦.
