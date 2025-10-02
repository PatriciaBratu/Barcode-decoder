Project Overview
This project is a Scala implementation of a simplified EAN-13 barcode decoder. The decoder processes black-and-white images of barcodes (provided as .ppm files converted to binary .pbm matrices) and extracts the 13-digit numeric codes representing products. The system can be used by stores to identify products and calculate prices from their database.
How It Works
The decoder receives a matrix of 0s and 1s, where 1 represents a black pixel and 0 a white pixel.
Consecutive identical bits are grouped to determine the widths of bars in the barcode.
Special sequences for start, center, and stop bars are identified to isolate the 12 main digits.
Each digit is decoded by comparing the relative widths of 4-bar groups against standard EAN-13 encodings (L, G, R).
The first digit is inferred from the parity pattern of the first 6 digits.
The check digit is calculated and verified to ensure barcode validity.
Key Features
Implements run-length encoding and scaling for relative bar widths.
Supports parity-based decoding for the left and right groups of digits.
Validates barcodes using parity and check digit rules.
Modular design with higher-order functions for list processing and fraction arithmetic.
Technologies Used
Scala for functional and object-oriented programming.
EAN-13 barcode standard for decoding logic.
Usage
Input: .ppm barcode images converted to binary matrices.
Output: The 13-digit EAN-13 code as a string, if valid.
