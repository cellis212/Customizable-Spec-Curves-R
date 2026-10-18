# Example: a conservative original construction (Pastor and Stambaugh, 2003)

Long-short returns of the Pastor-Stambaugh liquidity beta (BetaLiquidityPS), whose original-paper construction is value-weighted, under the constructions published by Chen and Zimmermann (2022), October 2025 release (data v2.0.0). The predictor was chosen by a rule fixed before any return was viewed: among clear continuous predictors whose original construction is value-weighted (the conventionally conservative choice), the one with the most-cited original paper.

## Data (not redistributed)

Download these files from the October 2025 Open Source Asset Pricing release into `RAW/cz_2025/` (`curl -L "https://drive.usercontent.google.com/download?id=ID&export=download&confirm=t"`), and write a `provenance.json` with a `files` object mapping each name to `{"sha256": ...}`. The build script refuses files whose hashes differ.

| File | Drive ID | SHA-256 |
|---|---|---|
| PredictorPortsFull.csv | 1g7w-yQ6Cg2qbMEkER9Q3vgns4JszXQo6 | 6cdaf0ff051222b813e6f8404d7a30b94e146b421625f202bd04473fe97a1817 |
| PredictorAltPorts_Deciles.zip | 14x09ofL70ZKSB6YMNM9DSEJQwGU2Rwmj | 215f45d63e53a6cafd49ed0468aa5ec5d02ad524c453d6a8507f327da67e95ab |
| PredictorAltPorts_DecilesEW.zip | 1pFBX0LTyoSwH7i8mT4kPCS_3QghlbODD | b49364d5ddaa3ffa789b67b0757e3e38bc4291473510751593cae3273e5ee103 |
| PredictorAltPorts_DecilesVW.zip | 1_1WWZqilrt1gleeyAFwjv5aobd0QRbS3 | d1790e831b621069379d5584d3114292ac52e8cdc36e44e302e880bd0c972b7c |
| PredictorAltPorts_HoldPer_1.zip | 1Hr10yn1XRq-AYvKpfNLMy5hxY8oOyKYy | 1f08b5856d58c549529a22372b790905bc1b92711f5a149a4dacb2458411ece4 |
| PredictorAltPorts_HoldPer_12.zip | 1tjiW2IpogUpujNNgoFAcCk_Yedpfsr-H | dcdadcc6af1ef22e98b50987b994ea96d24eedba56ee84218b5465322e1a8502 |
| PredictorAltPorts_HoldPer_3.zip | 1pri4os1PUtpuimffXoS37iOGn9xSK48h | 3e94f0d127b3e1808da6c40c7fc1740ad28d5dde97ff8bd1724435b5dc416fe2 |
| PredictorAltPorts_HoldPer_6.zip | 1Fl44F1GEk8nS9uJpyVWZKhxWYS0CWV23 | a35e7896f46f3f08da88a4f19792047100e0e9e4eb6f369111a343a735af2ff9 |
| PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct.zip | 1Q4YatQ3soRU_V7VeACwUn2bnCnmhDUI2 | 6a889fe96fd445124305f529a1d91f19d94ca9c9f2541ff53c598c7deba61322 |
| PredictorAltPorts_LiqScreen_NYSEonly.zip | 1menxh3LzP5Fh2e21201HDAOUUNz475MD | eb3155f8f18427083d2597e2aa980b16f6e15ec355caa0a02c94036a56dccf6b |
| PredictorAltPorts_LiqScreen_Price_gt_5.zip | 1mL44YJHwiLt_ZRdjmiU7-_i4QVtWURLD | 7e457d36b7bcbd8c6a472d263c2df5244fcf7a2e60747e32db7c690b3620e99f |
| PredictorAltPorts_LiqScreen_VWforce.zip | 1KZE3FgBxFPaNOyxoRw63ubZHOlkR9kZW | 53d3ae1f3de7a17d051de0125da2c42e4d4cf664faf6e4f6cfdd3b4994675a2a |
| PredictorAltPorts_Quintiles.zip | 1n76lfnpblK4jH6SObZew_o7Qi1D3CXWz | b7d5f994ec32160a9f2183407923068d17542e7cc0a7934a769ae353e0c47d79 |
| PredictorAltPorts_QuintilesEW.zip | 1KjptjrRi96ko_tR8zFjXCyYfNlyiHGiI | 2be3c54459f5de8ee1bf4a474b9a3ea7a74cb3d37b8c690ade0dff773b77a0c6 |
| PredictorAltPorts_QuintilesVW.zip | 1ef905SSlCDyh1KU9W1tJs5sfBFz0HPUt | 03c2358f223670b6ce21853f2f9d86404012f6cea9a14e76d116ca05f20d5070 |

## Files

- `build_input.R` streams the BetaLiquidityPS long-short rows from each file, drops files that reproduce another construction exactly (Deciles, DecilesVW, HoldPer_12, LiqScreen_Price_gt_5, and VWforce equal the original construction; QuintilesVW equals Quintiles), and labels each construction by breakpoints, weighting, rebalancing, and screen.
- `menu.json` is the frozen menu `cz-bliq-v1`: two period panels (1968-1999; 2000-2024) with nine computable constructions each; 78 combinations that the public files cannot produce are exclusions. Baseline: Chen and Zimmermann's posted in-sample replication in this release, 0.37% per month (t = 2.03); their 2022 table reports 0.35 (t = 1.93) from an earlier data vintage.
