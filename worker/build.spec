
app name: BenchmarkApp
boards:
 - cherry-v5
 - cherry
build dir: $(SPEC_DIR)/build/$(BOARD)
define:
 - CC26XX_VIMS_GPRAM_MODE=1
 - BENCHMARK_VIMS_LB_DIS=0
 - BENCHMARK_WRITE_BUFFER_DIS=1
 - BENCHMARK_ID="ca5093cf-22d8-47d9-a6b0-15f5f0e4f3f1"
