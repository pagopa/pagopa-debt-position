package it.gov.pagopa.debtposition.controller.pd.validator;

import it.gov.pagopa.debtposition.model.pd.TransferModel;
import jakarta.validation.ConstraintValidatorContext;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
class ValidTransferListValidatorTest {

    private final ValidTransferListValidator validTransferListValidator = new ValidTransferListValidator();

    private final ConstraintValidatorContext context = Mockito.mock(ConstraintValidatorContext.class);

    private final ConstraintValidatorContext.ConstraintViolationBuilder builder = Mockito.mock(ConstraintValidatorContext.ConstraintViolationBuilder.class);

    @BeforeEach
    public void init(){
        Mockito.when(context.buildConstraintViolationWithTemplate(Mockito.anyString())).thenReturn(builder);
    }

    public static Stream<Arguments> invalidTransferModelIdsListSizeTestMethodSource(){
        return Stream.of(
                Arguments.of(List.of()),
                Arguments.of(IntStream.range(1, 7).boxed().map(String::valueOf).toList())
        );
    }

    @ParameterizedTest
    @MethodSource("invalidTransferModelIdsListSizeTestMethodSource")
    void shouldReturnFalseWhenTransferHaveWrongSize(List<String>transferModelIdsList) {
        List<TransferModel> transferModelList = transferModelIdsList.stream().map(id -> {
            TransferModel model = new TransferModel();
            model.setIdTransfer(id);
            return model;
        }).toList();
        assertFalse(validTransferListValidator.isValid(transferModelList, context));
        Mockito.verify(context).buildConstraintViolationWithTemplate("Transfer list must contain between 1 and 5 transfers");

    }

    public static Stream<Arguments> invalidTransferModelIdsTestMethodSource(){
        return Stream.of(
                Arguments.of("0"),
                Arguments.of("invalid"),
                Arguments.of("6")
        );
    }
    @ParameterizedTest
    @MethodSource("invalidTransferModelIdsTestMethodSource")
    void shouldReturnFalseWhenTransferListHaveInvalidIds(String invalidId) {
        TransferModel transferModel = new TransferModel();
        transferModel.setIdTransfer(invalidId);
        assertFalse(validTransferListValidator.isValid(List.of(transferModel), context));
        Mockito.verify(context).buildConstraintViolationWithTemplate("Transfer list contains invalid transfer ids: [%s]".formatted(invalidId));
    }

    @Test
    void shouldReturnFalseWhenTransferListHaveDuplicatedIds() {
        List<TransferModel> transferModelList = new ArrayList<>();
        TransferModel transferModel = new TransferModel();
        transferModel.setIdTransfer("1");
        transferModelList.add(transferModel);
        transferModelList.add(transferModel);
        assertFalse(validTransferListValidator.isValid(transferModelList, context));
        Mockito.verify(context).buildConstraintViolationWithTemplate("Transfer list invalid value: [1], expected: [2]");
    }

    public static Stream<Arguments> invalidTransferListMissingIdsMethodSource(){
        return Stream.of(
                Arguments.of(List.of("1", "3"),  "Transfer list invalid value: [3], expected: [2]"),
                Arguments.of(List.of("1", "2", "4"),  "Transfer list invalid value: [4], expected: [3]"),
                Arguments.of(List.of("1", "2", "3", "5"),  "Transfer list invalid value: [5], expected: [4]")
        );
    }
    @ParameterizedTest
    @MethodSource("invalidTransferListMissingIdsMethodSource")
    void shouldReturnFalseWhenTransferListWithMissingId(List<String> transferModelIdsList, String expectedErrorMessage) {
        List<TransferModel> transferModelList = transferModelIdsList.stream().map(id -> {
            TransferModel transferModel = new TransferModel();
            transferModel.setIdTransfer(id);
            return transferModel;
        }).toList();
        assertFalse(validTransferListValidator.isValid(transferModelList, context));
        Mockito.verify(context).buildConstraintViolationWithTemplate(expectedErrorMessage);
    }

    public static Stream<Arguments> validTransferModelTestMethodSource() {
        List<List<String>> validIdsSequences = new ArrayList<>();
        IntStream.range(1,6).forEach(i -> validIdsSequences.add(IntStream.range(1,i+1)
                .boxed()
                .map(String::valueOf)
                .toList()));

        return validIdsSequences.stream()
                .map(Arguments::of);
    }

    @ParameterizedTest
    @MethodSource("validTransferModelTestMethodSource")
    void shouldReturnTrueWhenTransferHaveValidIds(List<String>validIds) {
        List<TransferModel> transferModelList = validIds.stream().map(id -> {
            TransferModel model = new TransferModel();
            model.setIdTransfer(id);
            return model;
        }).toList();
        assertTrue(validTransferListValidator.isValid(transferModelList, context));
    }
  
}