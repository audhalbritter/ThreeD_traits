## Repository for the paper Halbritter et al. (submitted) Effects of warming, nitrogen, and grazing on plant functional traits differ between alpine and sub-alpine grasslands.

This is the README file for the repository associated with the paper Halbritter et al. (submitted) Effects of warming, nitrogen, and grazing on plant functional traits differ between alpine and sub-alpine grasslands.

### Reproducible Analysis Pipeline

This project uses the R packages **renv** and **targets** to ensure fully reproducible analyses. This README explains how to set up your environment, install dependencies, and run the analysis pipeline on any major platform.

---

## 1. Project Structure

- **renv**: Manages R package dependencies in a project-local library, ensuring reproducibility.
- **targets**: Orchestrates the analysis pipeline, making workflows reproducible and scalable.

---

## 2. System Requirements

### Operating Systems
- **macOS** (Intel/x86_64 or Apple Silicon/arm64)
- **Linux** (Debian/Ubuntu, Fedora, etc.)
- **Windows**

### System-level Dependencies
The following system libraries must be installed **before** restoring the R environment:

- **OpenSSL**
- **freetype**
- **harfbuzz**
- **fribidi**
- **libtiff**
- **jpeg**
- **pkg-config** (for compiling R packages)
- **A C/C++ compiler toolchain** (e.g., Xcode Command Line Tools, build-essential, Rtools)

#### How to install system libraries:

- **macOS:**
  - Install [Homebrew](https://brew.sh/)
  - Install libraries:
    ```sh
    brew install pkg-config openssl freetype harfbuzz fribidi libtiff jpeg
    ```
  - If using Intel/x86_64 R on Apple Silicon, prefix with `arch -x86_64` as needed.

- **Linux (Debian/Ubuntu):**
    ```sh
    sudo apt-get update
    sudo apt-get install build-essential pkg-config libssl-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev libtiff5-dev libjpeg-dev
    ```
- **Linux (Fedora/RedHat):**
    ```sh
    sudo dnf install @development-tools pkgconf-pkg-config openssl-devel freetype-devel harfbuzz-devel fribidi-devel libtiff-devel libjpeg-devel
    ```
- **Windows:**
  - Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for compilation.
  - Most dependencies are available as binary packages from CRAN. If you encounter errors, install the corresponding system libraries or use precompiled binaries.

> **Note:** Ensure that the system libraries are available for the architecture matching your R installation (e.g., x86_64 or arm64).

---

## 3. Environment Variables

A project-local `.Renviron` file may be provided to help R find the correct system libraries. If you need to recreate it, add the following lines to `.Renviron` in the project root, adjusting paths for your platform:

```
PKG_CONFIG_PATH="/path/to/openssl/pkgconfig:/path/to/freetype/pkgconfig:/path/to/harfbuzz/pkgconfig:/path/to/fribidi/pkgconfig:/path/to/libtiff/pkgconfig:/path/to/jpeg/pkgconfig"
PATH="/path/to/openssl/bin:${PATH}"
```

- On macOS with Homebrew, the default is `/usr/local/opt/<package>/lib/pkgconfig` (Intel) or `/opt/homebrew/opt/<package>/lib/pkgconfig` (Apple Silicon).
- On Linux, use `/usr/lib/x86_64-linux-gnu/pkgconfig` or similar.
- On Windows, this is usually not needed if using binary packages.

After editing `.Renviron`, restart your R session.

---

## 4. Setting Up the R Environment

1. **Clone the repository** and open the project in RStudio or your preferred R environment.
2. **Install renv** (if not already installed):
   ```r
   install.packages("renv")
   ```
3. **Restore the project library:**
   ```r
   renv::restore()
   ```
   This will install all required R packages as specified in `renv.lock`.

---

## 5. Running the Analysis Pipeline

1. **Install targets** (if not already installed):
   ```r
   install.packages("targets")
   ```
2. **Run the pipeline:**
   ```r
   targets::tar_make()
   ```
   This will execute the workflow as defined in your `_targets.R` file and any supporting scripts in the `R/` directory.

---

## 6. Troubleshooting

- If you encounter errors about missing system libraries (e.g., OpenSSL, freetype, harfbuzz, fribidi, libtiff, jpeg), ensure you have installed the correct versions for your platform and architecture, and that your `PKG_CONFIG_PATH` is set up as above.
- If you see errors about package versions or missing packages, run `renv::status()` to diagnose and `renv::snapshot()` to update the lockfile if needed.
- For custom or GitHub packages, install them as needed (e.g., `remotes::install_github("audhalbritter/dataDocumentation")`) and then run `renv::snapshot()`.

---

## 7. Reproducibility

- All R package dependencies are tracked in `renv.lock`.
- All system-level dependencies are listed above.
- The analysis pipeline is fully reproducible using the `targets` workflow.

---

## 8. Platform-specific Notes

- **macOS:** If using Apple Silicon, you may need to use Intel (x86_64) versions of R and Homebrew for compatibility with some packages. Use `arch -x86_64` as a prefix for commands if needed.
- **Linux:** Package names may vary by distribution; consult your package manager's documentation if you encounter issues.
- **Windows:** Most dependencies are available as precompiled binaries. If you encounter compilation errors, ensure Rtools is installed and on your PATH.

---

## 9. Contact

For questions or issues, please contact the project maintainer or open an issue in the repository.
