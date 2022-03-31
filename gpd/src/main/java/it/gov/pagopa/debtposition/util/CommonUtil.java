package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.model.PageInfo;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.OrderType;
import lombok.experimental.UtilityClass;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;


@UtilityClass
public class CommonUtil {


    /**
     * Get the field name from enumerations that implements {@link OrderType}.
     * See {@link Order} class.
     * The field name identify the column
     *
     * @param filterAndOrder object with sorting info
     * @return a {@link Sort} object to use with SpringRepository
     */
    public Sort getSort(FilterAndOrder filterAndOrder) {
        return Sort.by(filterAndOrder.getOrder().getOrdering(), filterAndOrder.getOrder().getOrderBy().getColumnName());
    }

    /**
     * @param example filter
     * @return a new Example using the custom ExampleMatcher
     */
    public static <T> Example<T> getFilters(T example) {
        ExampleMatcher matcher = ExampleMatcher.matching()
                .withIgnoreNullValues()
                .withIgnoreCase(true)
                .withStringMatcher(ExampleMatcher.StringMatcher.CONTAINING);
        return Example.of(example, matcher);
    }

    /**
     * @param page Page returned from the database
     * @return return the page info
     */
    public <T> PageInfo buildPageInfo(Page<T> page) {
        return PageInfo.builder()
                .page(page.getNumber())
                .limit(page.getSize())
                .totalPages(page.getTotalPages())
                .itemsFound(page.getNumberOfElements())
                .build();
    }


}
