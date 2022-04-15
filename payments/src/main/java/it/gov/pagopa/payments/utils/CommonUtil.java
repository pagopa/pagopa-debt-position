package it.gov.pagopa.payments.utils;

import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.PageInfo;
import it.gov.pagopa.payments.model.PaymentsResult;
import lombok.experimental.UtilityClass;



@UtilityClass
public class CommonUtil {



    /**
     * @param receipts Page returned from the database
     * @return return the page info
     */

    public PageInfo buildPageInfo(PaymentsResult<ReceiptEntity> receipts) {
        return PageInfo.builder()
                .limit(receipts.getPageSize())
                .morePages(receipts.isHasMoreResults())
                .itemsFound(receipts.getLength())
                .page(receipts.getCurrentPageNumber())
                .build();
    }


}
