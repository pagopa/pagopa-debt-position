package it.gov.pagopa.payments.utils;

import com.microsoft.azure.storage.ResultSegment;

import it.gov.pagopa.payments.model.PageInfo;
import lombok.experimental.UtilityClass;



@UtilityClass
public class CommonUtil {



    /**
     * @param page Page returned from the database
     * @return return the page info
     */

    public <T> PageInfo buildPageInfo(ResultSegment<T> page) {
        return PageInfo.builder()
                .limit(page.getPageSize())
                .totalPages(page.getRemainingPageResults())
                .itemsFound(page.getLength())
                .build();
    }


}
