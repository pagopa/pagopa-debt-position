package it.gov.pagopa.debtposition.controller.pd.validator;


import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import it.gov.pagopa.debtposition.model.pd.TransferModel;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class ValidTransferListValidator
    implements ConstraintValidator<ValidTransferList, List<TransferModel>> {

  private static final Set<String> VALID_TRANSFER_ID_SET = IntStream
          .range(1, ValidTransferList.MAX_TRANSFER_LIST_SIZE + 1)
          .boxed()
          .map(String::valueOf)
          .collect(Collectors.toSet());



  @Override
  public boolean isValid(List<TransferModel> transferList, ConstraintValidatorContext context) {
    /*
     * a valid transfer list have to respect following constraint:
     * - size must be between 1 and 5 -> validated with Size annotation
     * - each transfer should have transferId between 1 and x and be unique (where x is the transfer list size)
     * - transfer ids should be progressively populated, so that there is no jump (f.e. 1, 3, 4, 5 is not allowed)
     */
    if(transferList == null || transferList.isEmpty() || transferList.size() > ValidTransferList.MAX_TRANSFER_LIST_SIZE){
      formatMessage(context, "Transfer list must contain between %s and %s transfers".formatted(1, ValidTransferList.MAX_TRANSFER_LIST_SIZE));
      return false;
    }
    List<String> transferIds = transferList.stream().map(TransferModel::getIdTransfer).toList();
    List<String> invalidIds = transferIds.stream().filter(Predicate.not(VALID_TRANSFER_ID_SET::contains)).toList();
    if(!invalidIds.isEmpty()){
      formatMessage(context, "Transfer list contains invalid transfer ids: " + invalidIds);
      return false;
    }
    int currentIdx = 1;
    for(String transferIdx: transferIds){
      //safe here, String ids can be one of integer value when here
      if(Integer.parseInt(transferIdx) != currentIdx){
        formatMessage(context, "Transfer list invalid value: [%s], expected: [%d]".formatted(transferIdx, currentIdx));
        return false;
      }
      currentIdx++;
    }
    return true;
  }

  private void formatMessage(ConstraintValidatorContext context, String validationErrorMessage) {
    context.disableDefaultConstraintViolation();
    context
        .buildConstraintViolationWithTemplate(validationErrorMessage)
        .addConstraintViolation();
  }
}
