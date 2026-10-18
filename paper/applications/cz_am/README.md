# Application 2: the assets-to-market anomaly (Fama and French, 1992)

Mean monthly long-short return of the AM (total assets to market equity) portfolio under the constructions published by Chen and Zimmermann (2022), April 2021 release (v1.1.0); Newey-West standard errors.

## Data (not redistributed)

Download these files from the April 2021 Open Source Asset Pricing release into `RAW/cz_2021/` (Google Drive IDs shown; the pattern `https://drive.usercontent.google.com/download?id=ID&export=download&confirm=t` works with `curl -L`):

| File | Drive ID | SHA-256 |
|---|---|---|
| PredictorPortsFull.csv | 10YTTmjqsXMB_gwmUyYiRPfATmh0fv8dr | f51caf0e55c13e4defb915d24c7ee4357d61bd63802af023458b78381b581ceb |
| PredictorAltPorts_Deciles.zip | 1Ptwu85VOsgBAivcR_o3Iy0IvXRbwn8dy | c86c76bdeb625a2a1f4ada3b837f5fff27d09c8084ac3a96ef23d8b6ad2ec468 |
| PredictorAltPorts_DecilesVW.zip | 11vaFRWAhEmL80tkP0L-QD-92McE1loQS | da46f9d5090ce0ad2d0db441f2064ef6238b7e5ba2a36dc925f14d64123ea2c4 |
| PredictorAltPorts_Quintiles.zip | 1vvK89jVJ-VIvcVjhLUfKxq2PaGGQVqiW | 4996bd5c6ba9aeb457b7ebd5fcc5e3bb215a868a9536f1ddade7e63a06dfa470 |
| PredictorAltPorts_QuintilesVW.zip | 1AH-dfwo6_HRphRa5MLW2y1TPwksaI_4- | b8bae8c2ae1a9ef67e537ed05f15237aea50527aa47df6d6cefc63c49cc14e49 |
| PredictorAltPorts_HoldPer_1.zip | 1GPtUOiqqTLjVTSzPNgNFddJp2vP0YWz2 | 4754b26312fef75f696e2d16bfca93b58fc1276e4934756c0d781bd9d7bc753d |
| PredictorAltPorts_HoldPer_3.zip | 1XtFUBodv0QfYpDMQj9m-fuxwrUGKXF5- | 673ff679f9b81cea5eab16793576ade6c9d20e26e06b937a5de587874c53aff1 |
| PredictorAltPorts_HoldPer_6.zip | 1zJVLRtqwt6MPGB1DQLqlg3bVT2VJ7-Ws | 8c712b591ad931b62441262aedaab6df3d5f059895739fd46bb8d97f2aabc91a |
| PredictorAltPorts_HoldPer_12.zip | 1lUYsF3RD9LHAtlIo97SqxrH200S_TcKb | ff3dcc5770a575aed86d53308b4a5c66c903cb6d1bf41f1b772c6525ca10bc82 |
| PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct.zip | 1xGn6zZYp2d5GnnLeSiFQwpBzCEwaU_9i | 142b2f0a77938ac5274e7bf898e9f67b5c2604b2e13d94fb26eb2d3536626328 |
| PredictorAltPorts_LiqScreen_Price_gt_5.zip | 1e4eSHFt4mcOqmBKOXXzkxDXxFuYia53V | a7121cbb4a2353a32ce1c6882f4eb02503d777607b1d0418ba414cc3c52d58f4 |
| PredictorAltPorts_LiqScreen_NYSEonly.zip | 1rFOmCCpok0r5bi50HaP73FFQjGrR6ktl | 57f9a1242aa7b3d66edfc43c5135cd17d365ec1657c588ffb6ce7d8984421a59 |
| PredictorAltPorts_LiqScreen_VWforce.zip | 1NUV3czB8pRftCLHqQndAPeh9LXQHVIYt | 4718289600bcc2a2795415e3af1e916fd9aa7277c373b7b8ccaeee6e885c2ac1 |

Write a `provenance.json` beside them with a `files` object mapping each file name to `{"sha256": ...}`; the build script refuses files whose hashes differ.

## Files

- `build_input.R` streams the AM long-short rows from each file, drops alternative files that reproduce another construction exactly (for AM: Quintiles and HoldPer_12 equal the original construction; VWforce equals QuintilesVW), and labels each construction by breakpoints, weighting, rebalancing, and liquidity screen.
- `menu.json` is the frozen menu `cz-am-v1`: two period panels (1963-1990; 1991-2020) and ten computable constructions each; 108 combinations that the public files cannot produce are listed as exclusions. Baseline: Chen and Zimmermann (2022) Table 2, AM, 0.63% per month.

After freezing, only the menu's `notes` field was edited (to point to the published Supplementary Material instead of an internal record); the grid, options, and reference are unchanged.
