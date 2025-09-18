package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.model.PageInfo;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.filterandorder.OrderType;
import java.util.List;
import java.util.Optional;
import lombok.experimental.UtilityClass;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

@UtilityClass
public class CommonUtil {

  /**
   * Get the field name from enumerations that implements {@link OrderType}. See {@link Order}
   * class. The field name identify the column
   *
   * @param filterAndOrder object with sorting info
   * @return a {@link Sort} object to use with SpringRepository
   */
  public Sort getSort(FilterAndOrder filterAndOrder) {
    return Sort.by(
        filterAndOrder.getOrder().getOrdering(),
        filterAndOrder.getOrder().getOrderBy().getColumnName());
  }

  /**
   * @param example filter
   * @return a new Example using the custom ExampleMatcher
   */
  public static <T> Example<T> getFilters(T example) {
    ExampleMatcher matcher =
        ExampleMatcher.matching()
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

  /**
   * @param list the page content
   * @param pageNumber the page number
   * @param pageSize the number of items per page
   * @param totalRecords the total amount of items available
   * @return return the page info
   */
  public static <T> Page<T> toPage(List<T> list, int pageNumber, int pageSize, long totalRecords) {
    PageRequest pageable = PageRequest.of(pageNumber, pageSize);
    return new PageImpl<>(list, pageable, totalRecords);
  }

  /**
   * @param segregationCode the segregationCode value
   * @return return the input string incremented to the next character
   */
  public static String getSegregationCodeEnd(String segregationCode) {
    int length = segregationCode.length() - 1;
    int nextChar = segregationCode.toCharArray()[length] + 1;
    return segregationCode.substring(0, length) + (char) nextChar;
  }

  /**
   *
   * @param nav
   * @param segregationCodes
   * @return
   */
  public static boolean isAuthorizedOnNavBySegregationCode(String nav, List<String> segregationCodes) {
    // It is enough to check only one NAV of the payment position. Here it is assumed that they all
    // have the same segregation code.
    String paymentPositionSegregationCode = nav.substring(1, 3);
    return segregationCodes.contains(paymentPositionSegregationCode);
  }

  /**
   * @param value value to deNullify.
   * @return return empty string if value is null
   */
  public static String deNull(Object value) {
    return Optional.ofNullable(value).orElse("").toString();
  }

  public static String sanitize(String input) {
    // Remove line-breaks, tabs, and anything non-alphanumeric/hyphen/asterisk
    return input == null ? null : input.replaceAll("[\\n\\r\\t]", "_").replaceAll("[^A-Za-z0-9\\-\\*]", "");
  }
}
