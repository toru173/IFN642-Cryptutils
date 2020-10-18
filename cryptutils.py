byte_size = 8
word_size = 32

def charToInt(char) :
    char = char.upper()
    if char == "A" : return 0
    if char == "B" : return 1
    if char == "C" : return 2
    if char == "D" : return 3
    if char == "E" : return 4
    if char == "F" : return 5
    if char == "G" : return 6
    if char == "H" : return 7
    if char == "I" : return 8
    if char == "J" : return 9
    if char == "K" : return 10
    if char == "L" : return 11
    if char == "M" : return 12
    if char == "N" : return 13
    if char == "O" : return 14
    if char == "P" : return 15
    if char == "Q" : return 16
    if char == "R" : return 17
    if char == "S" : return 18
    if char == "T" : return 19
    if char == "U" : return 20
    if char == "V" : return 21
    if char == "W" : return 22
    if char == "X" : return 23
    if char == "Y" : return 24
    if char == "Z" : return 25
    return -1


def intToChar(int) :
    if int == 0 : return "A"
    if int == 1 : return "B"
    if int == 2 : return "C"
    if int == 3 : return "D"
    if int == 4 : return "E"
    if int == 5 : return "F"
    if int == 6 : return "G"
    if int == 7 : return "H"
    if int == 8 : return "I"
    if int == 9 : return "J"
    if int == 10 : return "K"
    if int == 11 : return "L"
    if int == 12 : return "M"
    if int == 13 : return "N"
    if int == 14 : return "O"
    if int == 15 : return "P"
    if int == 16 : return "Q"
    if int == 17 : return "R"
    if int == 18 : return "S"
    if int == 19 : return "T"
    if int == 20 : return "U"
    if int == 21 : return "V"
    if int == 22 : return "W"
    if int == 23 : return "X"
    if int == 24 : return "Y"
    if int == 25 : return "Z"
    return ""


def hexToDec(hexadecimal) :
    if hexadecimal == "0" : return 0
    if hexadecimal == "1" : return 1
    if hexadecimal == "2" : return 2
    if hexadecimal == "3" : return 3
    if hexadecimal == "4" : return 4
    if hexadecimal == "5" : return 5
    if hexadecimal == "6" : return 6
    if hexadecimal == "7" : return 7
    if hexadecimal == "8" : return 8
    if hexadecimal == "9" : return 9
    if hexadecimal == "A" : return 10
    if hexadecimal == "B" : return 11
    if hexadecimal == "C" : return 12
    if hexadecimal == "D" : return 13
    if hexadecimal == "E" : return 14
    if hexadecimal == "F" : return 15
    return -1


def decToHex(decimal) :
    if decimal == 0 : return "0"
    if decimal == 1 : return "1"
    if decimal == 2 : return "2"
    if decimal == 3 : return "3"
    if decimal == 4 : return "4"
    if decimal == 5 : return "5"
    if decimal == 6 : return "6"
    if decimal == 7 : return "7"
    if decimal == 8 : return "8"
    if decimal == 9 : return "9"
    if decimal == 10 : return "A"
    if decimal == 11 : return "B"
    if decimal == 12 : return "C"
    if decimal == 13 : return "D"
    if decimal == 14 : return "E"
    if decimal == 15 : return "F"
    return ""


def decToBin(decimal) :
    if decimal > (2 ** byte_size - 1) or decimal < 0 :
        return None
    
    output_binary = []

    for i in range(byte_size):
        remainder = 2 ** (byte_size - (i + 1))
        if decimal - remainder < 0:
            output_binary.append(0)
        else :
            output_binary.append(1)
            decimal -= remainder

    return output_binary


def binToDec(binary) :
    if len(binary) != byte_size :
        return None

    output_decimal = 0

    for i in range(byte_size) :
        if not (binary[i] == 1 or binary[i] == 0) :
            return None
        if binary[i] :
            bit_index = byte_size - (i + 1)
            output_decimal += 2 ** bit_index

    return output_decimal


def unsignedExtend(bits) :
    bytes = []
    for i in range(byte_size - len(bits)) :
        bytes.append(0)

    for i in range(len(bits)) :
        bytes.append(bits[i])

    return bytes


def ASCIIToDec(ASCII_characer) :
    return ord(ASCII_characer)


def decToASCII(decimal) :
    return chr(decimal)


def ASCIIToByte(ASCII_characer) :
    return decToBin(ASCIIToDec(ASCII_characer))


def byteToASCII(binary) :
    return decToASCII(binToDec(binary))


def ASCIIToByteList(string) :
    output_list = []
    for i in range(len(string)) :
        output_list.append(ASCIIToByte(string[i]))

    return output_list


def hexStringToHexList(hexadecimal_string) :
    return hexadecimal_string.split(" ")


def hexListToHexString(hexadecimal_list) :
    output_string = hexadecimal_list[0]
    for i in range(1, len(hexadecimal_list)) :
        output_string += " " + hexadecimal_list[i]

    return output_string


def hexListToByteList(hexadecimal_list) :
    output_list = []
    for i in range(len(hexadecimal_list)) :
        output_list.append(hexToByte(hexadecimal_list[i]))

    return output_list


def hexToByte(hexadecimal) :
    return decToBin(hexToDec(hexadecimal[0]) * 16 + hexToDec(hexadecimal[1]))


def nybbleToHex(nybble) :
    return decToHex(binToDec(unsignedExtend(nybble)))


def byteToHex(byte) :
    first_nybble = byte[0 : 4]
    second_nybble = byte[4 : 8]
    
    return nybbleToHex(first_nybble) + nybbleToHex(second_nybble)


def byteListToHexList(byte_list) :
    output_list = []
    for i in range(len(byte_list)) :
        output_list.append(byteToHex(byte_list[i]))

    return output_list


def bytewiseXOR(byte_one, byte_two) :
    output_byte = []
    
    for i in range(byte_size) :
        output_byte.append(byte_one[i] ^ byte_two[i])

    return output_byte


def bytewiseOR(byte_one, byte_two) :
    output_byte = []
    
    for i in range(byte_size) :
        output_byte.append(byte_one[i] | byte_two[i])
    
    return output_byte


def bytewiseAND(byte_one, byte_two) :
    output_byte = []
    
    for i in range(byte_size) :
        output_byte.append(byte_one[i] & byte_two[i])
    
    return output_byte


def isValidCharacter(char) :
    return charToInt(char) >= 0


def isValidInt(int) :
    return intToChar(int) is not ""


def byteListToString(byte_list) :
    byte_string = ""
    for i in range(len(byte_list)) :
        for bit in range(byte_size) :
            byte_string += str(byte_list[i][bit])
            if bit == 3 :
                byte_string += " "
        if i < len(byte_list) - 1 :
            byte_string += "  "

    return byte_string


def byteListToASCII(byte_list) :
    output_string = ""
    for i in range(len(byte_list)) :
        output_string += byteToASCII(byte_list[i])

    return output_string


def bitsToBlockList(bit_list, block_size) :
    output_block_list = []

    while (len(bit_list) % block_size) :
        # Pad bit list with zeroes if not multiple of block_size
        bit_list.append(0)
    
    for blocks in range(0, bit_list, block_size) :
        output_block = []
        for bits in range(block_size) :
            output_block.append(bit_list[bits + blocks])

        output_block_list.append(output_block)

    return output_block_list



def bitsToByteList(bit_list) :
    output_byte_list = []
    
    while (len(bit_list) % byte_size) :
        # Pad bit list with zeroes if not multiple of byte_size
        bit_list.append(0)
    
    for bytes in range(0, len(bit_list), byte_size):
        output_byte = []
        for bits in range(byte_size) :
            output_byte.append(bit_list[bits + bytes])
                
        output_byte_list.append(output_byte)

    return output_byte_list


def byteListToBits(byte_list) :
    output_list = []

    for i in range(len(byte_list)) :
        for j in range(byte_size) :
            output_list.append(byte_list[i][j])

    return output_list


def stringToBitList(string) :
    output_list = []
    for i in range(len(string)) :
        output_list.append(ASCIIToByte(string[i]))

    return byteListToBits(output_list)


def bitListToString(bit_list) :
    output_string = ""
    for i in range(len(bit_list)) :
        output_string += str(bit_list[i])

    return output_string


def stringToList(string) :
    output_list = []
    for i in range(len(string)) :
        output_list.append(string[i])
    return output_list


def listToString(list) :
    output_string = ""
    for i in range(len(list)) :
        output_string += str(list[i])
    return output_string


def stripPunctuation(string) :
    output_string = ""
    for character in range(len(string)) :
        if isValidCharacter(string[character]) :
            output_string += string[character]
    return output_string


def getDictionaryIndex(dictionary, character) :
    for i in range(len(dictionary)) :
        if dictionary[i] == character :
             return i
    return -1


def isInDictionary(dictionary, character) :
    return getDictionaryIndex(dictionary, character) > -1


def getCharacterFrequency(string, ignore_punctuation = True) :
    dictionary = []
    frequency = []

    string = string.upper()

    for new_character in range(len(string)) :
        if isInDictionary(dictionary, string[new_character]) :
            if ignore_punctuation == True :
                if isValidCharacter(string[new_character]) :
                    frequency[getDictionaryIndex(dictionary, string[new_character])] += 1
            else :
                frequency[getDictionaryIndex(dictionary, string[new_character])] += 1
        else :
            if ignore_punctuation == True :
                if isValidCharacter(string[new_character]) :
                    dictionary.append(string[new_character])
                    frequency.append(1)
            else :
                dictionary.append(string[new_character])
                frequency.append(1)

    return dictionary, frequency


def getHighestFrequency(string, ignore_punctuation = True) :
    dictionary, frequency = getCharacterFrequency(string, ignore_punctuation)

    return sorted(frequency)[-1]


def getMostFrequentCharacter(string, ignore_punctuation = True) :
    dictionary, frequency = getCharacterFrequency(string, ignore_punctuation)
    
    max_frequency = 0
    max_frequency_character = 0

    for character in range(len(frequency)) :
        if frequency[character] > max_frequency :
            max_frequency = frequency[character]
            max_frequency_character = dictionary[character]

    return max_frequency_character


def getNumUniqueCharacters(string, ignore_punctuation = True) :
    dictionary, frequency = getCharacterFrequency(string, ignore_punctuation)
    return sum(frequency)


def getSingleCharacterFrequency(string, character, ignore_punctuation = True) :
    dictionary, frequency = getCharacterFrequency(string, ignore_punctuation)
    
    unique_characters = getNumUniqueCharacters(string, ignore_punctuation)
    
    for i in range(unique_characters) :
        if dictionary[i] == character :
            return frequency[i] / (unique_characters * 1.0)
    return -1


def drawFrequencyHistogram(string, ignore_punctuation = True) :
    dictionary, frequency = getCharacterFrequency(string, ignore_punctuation)

    y_axis = getHighestFrequency(string, ignore_punctuation)
    
    for y in reversed(range(1, y_axis + 1)) :
        output_string = str(y)
        output_string += (2 - len(str(y))) * " "
        output_string += "|"
        for x in range(len(dictionary)) :
            if frequency[x] >= y:
                output_string += " #"
            else :
                output_string += "  "
        print output_string

    print "--+" + "--" * len(dictionary)

    output_string = "  |"

    for i in range(len(dictionary)) :
        output_string += " " + dictionary[i]

    print output_string


def ceasar_enrypt_string(string, offset) :
    encrypted_list = stringToList(string)
    
    # Convert to integers
    for character in range(len(encrypted_list)) :
        if isValidCharacter(encrypted_list[character]) :
            encrypted_list[character] = charToInt(encrypted_list[character])

    # Do encryption
    for character in range(len(encrypted_list)) :
        if isValidInt(encrypted_list[character]) :
            encrypted_list[character] = (encrypted_list[character] + offset) % 26


    # Convert back to characters
    for character in range(len(encrypted_list)) :
        if isValidInt(encrypted_list[character]) :
            encrypted_list[character] = intToChar(encrypted_list[character])
            if string[character].islower() :
                encrypted_list[character] = encrypted_list[character].lower()

    return listToString(encrypted_list)


def ceasar_decrypt_string(string, offset) :
    decrypted_list = stringToList(string)
    
    # Convert to integers
    for character in range(len(decrypted_list)) :
        if isValidCharacter(decrypted_list[character]) :
            decrypted_list[character] = charToInt(decrypted_list[character])

    # Do encryption
    for character in range(len(decrypted_list)) :
        if isValidInt(decrypted_list[character]) :
            decrypted_list[character] = (decrypted_list[character] - offset) % 26

    # Convert back to characters
    for character in range(len(decrypted_list)) :
        if isValidInt(decrypted_list[character]) :
            decrypted_list[character] = intToChar(decrypted_list[character])
            if string[character].islower() :
                decrypted_list[character] = decrypted_list[character].lower()
    
    return listToString(decrypted_list)


def make2DMatrix(rows, columns) :
    return [[0] * columns for i in range(rows)]


def transposition_encrypt_string(string, key) :
    
    character_list = stringToList(string)
    
    key_length = len(key)
    columns = key_length
    rows = (len(string) + (columns - 1)) // columns
    
    # Pad any missing characters with the null character, "Z"
    for i in range((rows * columns) - len(character_list)) :
        character_list.append("Z")
    
    # Convert string to matrix with the number "key_length" columns
    # Ensure to populate row at a time - "(i * columns) + j"
    transposition_matrix = make2DMatrix(rows, columns)
    for i in range(rows) :
        for j in range(columns) :
            transposition_matrix[i][j] = character_list[(i * columns) + j]

    # Re-order the columns. Write out by reading down columns
    encrypted_list = []
    for j in range(columns) :
        for i in range(rows) :
            encrypted_list.append(transposition_matrix[i][key[j]])
    
    return listToString(encrypted_list)


def transposition_decrypt_string(string, key) :
    
    character_list = stringToList(string)
    
    key_length = len(key)
    columns = key_length
    rows = (len(string) + (columns - 1)) // columns
    
    # Convert string to matrix with the number "key_length" columns.
    # Ensure to populate column at a time - "i + (j * rows)"
    transposition_matrix = make2DMatrix(rows, columns)
    for i in range(rows) :
        for j in range(columns) :
            transposition_matrix[i][j] = character_list[i + (j * rows)]

    # Re-order the columns. Write out by reading across the rows
    decrypted_list = []
    for i in range(rows) :
        for j in range(columns) :
            decrypted_list.append(transposition_matrix[i][key[j]])

    for i in range(rows) :
        output_string = " "
        for j in range(columns) :
            output_string += transposition_matrix[i][key[j]]
        print output_string
    
    return listToString(decrypted_list)


def factorial(number) :
    if number == 1 :
        return number
    else:
        return number * factorial(number - 1)


def getFactors(number) :
    # As failure is always an options; likewise,  1 is always a factor
    factor_list = [1]

    # Highest factor will be at sqrt(x)
    for factor in range(2, int((number ** 0.5) + 1)) :
        if not (number % factor) :
            # number % factor = 0 indicates that int can be divided by factor without remainder
            relatively_prime_factors = getFactors(factor)
            common_factors = 0
            for j in range(len(factor_list)) :
                for i in range(len(relatively_prime_factors)) :
                    if i == j:
                        common_factors += 1

            if common_factors == 1 :
                # Only add factor if relativley prime to existing factors
                factor_list.append(factor)
            elif relatively_prime_factors[-1] == factor :
                # Add if factor is prime of it's own accord
                factor_list.append(factor)

    if factor_list == [1] and number > 1 :
        # Number must be prime. Add to list
        factor_list.append(number)

    return factor_list


def isPrime(number) :
    return getFactors(number)[-1] == number


def getTrigramFrequency(string) :
    # Trigram will either start at 0, 1 or 2. It will repeat after.
    
    string_length = len(string)
    trigram_list = []
    num_trigrams = (string_length + 2) // 3
    
    print num_trigrams
    
    for i in range(num_trigrams) :
        trigram_list.append("")
    
    for offset in range(3) :
        for trigram in range(offset, len(string), 3) :
            for character in range(3) :
                print offset + trigram + character
                trigram_list[trigram + offset] += string[offset + trigram + character]


def getKasiskiFrequency(string, factor) :
    # TODO: - Count number of occurances better
    #       - Fold in number of contributing factors
    
    min_length = 2
    string = string.upper()
    
    if factor < min_length :
        # Look for minimum of 3 repeating characters
        return 0
    
    factor_list = getSubstringsByCharacterCount(string, factor)
    
    num_factors = len(factor_list)

    for i in range(num_factors) :
        # trim to the first min_length characters
        factor_list[i] = factor_list[i][0 : min_length + 1]

    highest_factors = 0
    highest_repitition = ""
    
    print factor_list
    
    repeated_factors = []

    for i in range(num_factors) :
        for j in range(num_factors) :
            if i is not j :
                if factor_list[i] == factor_list[j] :
                    repeated_factors.append(factor_list[j])

    print repeated_factors

#    print highest_repitition
    return len(repeated_factors)


def getSubstringsByCharacterCount(string, character_count) :
    # Example: print getSubstringsByCharacterCount("ABCDEFGHI", 3)
    #          ["ABC", "DEF", "GHI"]
    substrings = []

    num_substrings = (len(string) + (character_count - 1)) // character_count

    for i in range(num_substrings) :
        substrings.append("")

    for string_number in range(num_substrings) :
        current_string_index = string_number * character_count
        next_string_index = current_string_index + character_count
        max_string_index = len(string)
        if next_string_index > max_string_index :
            next_string_index = max_string_index
        
        for character in range(current_string_index, next_string_index) :
            substrings[string_number] += string[character]

    return substrings


def getSubstringsByCharacterStride(string, character_stride) :
    # Example: print getSubstringsByCharactersStride("ABCDEFGHI", 3)
    #          ["ADG", "BEH", "CFI"]
    substrings = []
    
    for i in range(character_stride) :
        substrings.append("")
    
    for string_number in range(character_stride) :
        for character in range(string_number, len(string), character_stride) :
            substrings[string_number] += string[character]

    return substrings


def vigenere_enrypt_string(string) :
    pass


def vigenere_derypt_string(string) :
    pass


def rounded_float(number, precision) :
    number_string = str(number)
    output_string = ""
    decimal_index = -1
    
    for numeral in range(len(number_string)) :
        output_string += number_string[numeral]
        if number_string[numeral] == "." :
            decimal_index = numeral
            break

    if decimal_index >= 0 :
        for numeral in range(precision) :
            number_string_index = decimal_index + numeral + 1
            if number_string_index >= len(number_string) :
                output_string += "0"
            else :
                output_string += number_string[number_string_index]

    return float(output_string)


def multiplyMatrixByConstant(matrix, constant) :
    rows = len(matrix)
    columns = len(matrix[0])
    for i in range(rows) :
        for j in range(columns) :
            matrix[i][j] = matrix[i][j] * constant
    return matrix


def getInverse2x2Matrix(matrix) :
    # Trivial case, hard coded. This is all we need for now.
    # A more general should be written!
    # https://www.mathsisfun.com/algebra/matrix-inverse.html
    inverse = make2DMatrix(2, 2)
    a = matrix[0][0] * 1.0
    b = matrix[0][1] * 1.0
    c = matrix[1][0] * 1.0
    d = matrix[1][1] * 1.0
    
    determinate = 1 / ((a * d) - (b * c))

    inverse[0][0] = d
    inverse[0][1] = -b
    inverse[1][0] = -c
    inverse[1][1] = a
    
    inverse = multiplyMatrixByConstant(inverse, determinate)

    return inverse

def multiplyMatrixByMatrix(matrix_a, matrix_b) :
    # https://www.mathsisfun.com/algebra/matrix-multiplying.html

    rows_a = len(matrix_a)
    columns_a = len(matrix_a[0])
    
    rows_b = len(matrix_b)
    columns_b = len(matrix_b[0])
    
    output_matrix = make2DMatrix(rows_a, columns_b)
    
    for i in range(rows_a) :
        for j in range(columns_b) :
            for k in range(columns_a) :
                output_matrix[i][j] += matrix_a[i][k] * matrix_b[k][j]

    return output_matrix

def matrixModulo26(matrix) :
    rows = len(matrix)
    columns = len(matrix[0])

    for i in range(rows) :
        for j in range(columns) :
            matrix[i][j] = matrix[i][j] % 26

    return matrix


def getInverseHillMatrix(matrix) :
    inverse = make2DMatrix(2, 2)
    a = matrix[0][0]
    b = matrix[0][1]
    c = matrix[1][0]
    d = matrix[1][1]
        
    if (((a * d) % 26) - ((b * c) % 26)) != 1 :
        # Determinate will not be one. Return null
        return None

    inverse[0][0] = d
    inverse[0][1] = (-b) % 26
    inverse[1][0] = (-c) % 26
    inverse[1][1] = a

    return inverse


def getHillMatrixMultiple(matrix_a, matrix_b) :
    return matrixModulo26(multiplyMatrixByMatrix(matrix_a, matrix_b))


def binaryAdditiveStreamCipherEncrypt(plaintext_byte_list, key_byte_list) :
    ciphertext_byte_list = []
    key_length = len(key_byte_list)
    for i in range(len(plaintext_byte_list)) :
        ciphertext_byte_list.append(bytewiseXOR(plaintext_byte_list[i], key_byte_list[i % key_length]))

    return ciphertext_byte_list


def binaryAdditiveStreamCipherDecrypt(ciphertext, key) :
    return binaryAdditiveStreamCipherEncrypt(ciphertext, key)

class LFSR :
    # Assumes len(seed) = len(taps)
    # Binary seed in format "[1, 0, 1, 1, 0, 1, ... ]"
    # Binary tap represented in format "[0, 0, 1, 0, 0, 1, ... ]"
    
    def __init__(self, seed, taps) :
        self.seed = seed
        self.taps = taps
        self.lfsr_length = len(self.seed)
        
        self.lfsr_state = []
        for i in range(self.lfsr_length) :
            self.lfsr_state.append(self.seed[i])


    def __str__(self) :
        return str(self.getState())
   

    def getState(self) :
        return self.lfsr_state
    
    
    def getOutput(self) :
        return self.lfsr_state[0]


    def shift(self) :
        self.input_bit = 0
    
        for i in range(self.lfsr_length) :
            if self.taps[i] :
                self.input_bit ^= self.lfsr_state[i]

        for i in range(1, self.lfsr_length) :
            self.lfsr_state[i - 1] = self.lfsr_state[i]

        self.lfsr_state[self.lfsr_length - 1] = self.input_bit


    def getMaxPeriod(self):
        return (2 ** self.lfsr_length) - 1
    
    
    def isMaximalPeriod(self) :
        return self.getMaxPeriod() == self.getPeriod()


    def getPeriod(self) :
        self.max_period = self.getMaxPeriod()

        i_lfsr = LFSR(self.seed, self.taps)
        for i in range(self.max_period + 1) :
            # Create two shift registers. Shift both until states are the same.
            # Calculate difference between number of shifts required. Takes in to
            # account LFSRs that do NOT settle on seed value
            j_lfsr = LFSR(self.seed, self.taps)
            for j in range(self.max_period + 1) :
                if i > j :
                    if i_lfsr.getState() == j_lfsr.getState():
                        return i - j
                j_lfsr.shift()
            i_lfsr.shift()


def getIntegerModuloExponentiation(integer, power, modulo) :
    # Uses Square and Multiply method
    if power == 0 :
        return 1
    if power == 1 :
        return integer % modulo
    if power % 2 :
        # Power is odd. Multiply by input integer
        return (getIntegerModuloExponentiation(integer, power - 1, modulo) * integer) % modulo
    else :
        # Power is even. Square input integer
        return (getIntegerModuloExponentiation(integer, power / 2, modulo) ** 2) % modulo


def getBinaryModuloExponentiation(bit_list, power, modulo) :
    # Creates list of factors up to 2 ^ (MSB)
    pass


def euclid(x, y) :
    # Returns greatest common divisor of x and y
    # https://web.stanford.edu/~dntse/classes/cs70_fall09/cs70_fall09_5.pdf
    if y == 0 :
        return x
    return euclid(y, x % y)


def extendedEuclid(x, y) :
    # Returns multiplicative inverse of x modulo y
    # https://web.stanford.edu/~dntse/classes/cs70_fall09/cs70_fall09_5.pdf
    if x == 0 :
        return None
    if y == 0 :
        return 1, 0
    a, b = extendedEuclid(y, x % y)
    return b, a - (x / y) * b


def getModuloInverse(x, modulo) :
    inverse = extendedEuclid(x, modulo)
    if inverse is not None :
        return inverse[0] % modulo
    return None


def getScatterplot(x_y_scatterplot_list, num_elements) :
    # Returns, as a string, a scatterplot of elements provided in the list
    # x_y_scatterplot_list and graphs them in the range 0 to (num_elements - 1)
    output_string = ""
    
    output_line = "  |"
    for x in range(num_elements) :
        if x < 10 :
            output_line += " " + str(x) + " "
        else:
            output_line += " " + str(x)

    output_string = output_line + output_string

    output_line = "--+" + "---" * num_elements + "\n"
    output_string = output_line + output_string

    for y in range(num_elements) :
        if y < 10 :
            output_line = str(y) + " |"
        else:
            output_line = str(y) + "|"

        for x in range(num_elements) :
            if [x, y] in x_y_scatterplot_list :
                output_line += " X "
            else :
                output_line += "   "

        output_string = output_line + "\n" + output_string

    return output_string


class ELLIPTIC_CURVE :
    # Class for manipulating elliptic curves
    def __init__(self, equation, modulo) :
        # elliptic curve satisfies y ^ 2 = x ^ 3 + ax + b mod p
        # Equation should be of the form (x ** 3) + (a * x) + b
        # Modulo will be represented as p
        self.equation = equation
        self.p = modulo
        # b is found at x = 0
        x = 0
        self.b = eval(equation)
        # a + b + a is found at x = 1; can can be found by a = f(0) - b - 1
        x = 1
        self.a = eval(equation) - self.b - 1
        
        self.x_points_list = []
        for x in range(modulo) :
            self.x_points_list.append(((x ** 3) + (self.a * x) + self.b) % self.p)

        self.y_points_list = []
        for y in range(modulo) :
            self.y_points_list.append((y ** 2) % self.p)

        self.x_y_point_list = []
        for x in range(self.p):
            for y in range(self.p) :
                if self.x_points_list[x] == self.y_points_list[y] :
                    self.x_y_point_list.append([x, y])

    def getXPointsList(self) :
        return self.x_points_list

    def getYPointsList(self) :
        return self.y_points_list

    def getXYPointsList(self) :
        return self.x_y_points_list

    def getOrder(self) :
        # Total of all points, plust point at infinity
        return len(self.x_y_points_list) + 1

    def getEquation(self) :
        return self.equation

    def getModulo(self) :
        return self.p
    
    def getNegationOperation(self, P) :
        return [P[0], (-P[1]) % self.p]

    def getAdditiveOperation(self, P, Q) :
        # Need to return P + Q, or, where P == Q, 2Q
        # Checks for point at infinity
        
        # Define point at infinity as [-1, -1]. This value should never normally exist
        # apart from as a result of this calculation
        xO = -1
        yO = -1
        
        O = [xO, yO]
        
        xP = P[0]
        yP = P[1]
        
        xQ = Q[0]
        yQ = Q[1]

        if P == O :
            # Result is defined as Q: O + Q = Q
            return Q
        
        if Q == O :
            # Result is defined as P: P + O = P
            return P

        if Q == self.getNegationOperation(P) or P == self.getNegationOperation(Q) :
            # P + Q = O when Q = -P. Result is defined as point of infinity
            return O

        if P == Q :
            # Points are the same
            S = (3 * (xP ** 2) + self.a) * getModuloInverse(2 * yP, self.p)
    
        else:
            # Points differ
            S = (yQ - yP) * getModuloInverse(xQ - xP, self.p)
        
        # From whiteboard during lecture
        xR = (S ** 2) - xQ - xP
        yR = S * (xP - xR) - yP
        
        xR = xR % self.p
        yR = yR % self.p
        
        xR = int(xR)
        yR = int(yR)

        return [xR, yR]
    
    def getMultiplicativeOperation(self, P, k) :
        # Adds point P k times around elliptic curve
        point = P[:] # Copies P
        while k > 1 :
            if point is not None :
                point = self.getAdditiveOperation(point, P)
                k -= 1
            else :
                break
        return point






