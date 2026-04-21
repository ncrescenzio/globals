<div align="center">
  <h1>Globals Library</h1>
  <p><strong>A Modern Fortran Utility Library for Numerical and Computational Engineering</strong></p>
</div>

---

## 📖 About The Project

`globals` is a robust Fortran library providing essential utilities and common routines to streamline scientific computing and numerical simulations. It abstracts away generic tasks such as I/O error handling, JSON configuration parsing, execution timing, and mathematical data sequencing.

**Original library:** [enrico_facca/globals](https://gitlab.com/enrico_facca/globals)

### ✨ Key Features

- **Standardized Kind Declarations** (`modKindDeclaration`): Ensure consistent precision across your numerical applications.
- **Robust Error Handling & I/O** (`modGlobals`): Simplified and safe file management and warning/error propagation.
- **JSON Configuration Parsing** (`modJsonData`): Fortran-friendly wrapper for `jsonfortran` to handle complex configuration files easily.
- **Timing & Performance Analysis** (`modTiming`, `modTimeInputs`, `modTimeOutputs`): Routines to profile and benchmark simulation segments.
- **Math & Utilities** (`modGaussQuadrature`, `modNorms`, `modDataSequence`): Standard computational tools out-of-the-box.

---

## 🗂️ Directory Structure

```text
globals/
├── cmake/            # Custom CMake configure scripts and find modules
├── examples/         # Usage examples showcasing library functionalities
├── srcs/             # Core Fortran source code modules
├── tests/            # Unit tests for the library
└── CMakeLists.txt    # Main CMake build configuration
```

---

## 🛠️ Dependencies

To build the library, you must have the following dependencies installed:

- **CMake**: >= 3.28
- **Fortran Compiler**: gfortran.
- **LAPACK & BLAS**: Linear algebra systems.
- **json-fortran**: A Fortran API for JSON data manipulation.

---

## 🚀 Getting Started

### 1. Clone the Repository

```bash
git clone git@github.com:ncrescenzio/globals.git
cd globals
```

### 2. Build the Library

The project primarily uses **CMake** for building. The default build type is `Release`. You can build the library using the following commands:

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . -j $(nproc)
```
> [!NOTE]
> For debugging and development, you can use `-DCMAKE_BUILD_TYPE=Debug`.

### 3. Install the Library

To install the library to your system or a specific directory, run:

```bash
cmake --install . --prefix /path/to/install/dir
```

### 4. Running Tests (Optional)

To build and run tests, you can enable testing by passing `-DGLOBALS_BUILD_TESTING=ON` to the initial configuration:

```bash
cmake .. -DGLOBALS_BUILD_TESTING=ON
cmake --build . -j $(nproc)
ctest
```

---

## 💻 Usage in Your Projects

Once installed, the `globals` library provides CMake export configurations. You can easily link it into your own custom projects!

Add this to your project's `CMakeLists.txt`:

```cmake
# Find the globals library package
find_package(globals REQUIRED)

# Add your executable
add_executable(my_app main.f90)

# Link against the globals library
target_link_libraries(my_app PRIVATE globals::globals)
```

If you installed to a custom prefix, be sure to add `-Dglobals_DIR=/path/to/install/dir/lib/cmake/globals` when configuring your own application.
