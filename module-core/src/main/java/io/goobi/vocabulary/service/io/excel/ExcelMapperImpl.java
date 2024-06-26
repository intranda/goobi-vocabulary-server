package io.goobi.vocabulary.service.io.excel;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

@Service
public class ExcelMapperImpl implements ExcelMapper {
    public static final int MAX_CELL_LENGTH = 32767;
    private final TabularRecordExporter tabularRecordExporter;

    public ExcelMapperImpl(TabularRecordExporter tabularRecordExporter) {
        this.tabularRecordExporter = tabularRecordExporter;
    }

    @Override
    public void toExcel(VocabularyEntity entity, OutputStream out) {
        List<List<String>> data = tabularRecordExporter.toTabularData(entity);
        try (Workbook wb = new XSSFWorkbook()) {
            Sheet sheet = wb.createSheet();
            int rowIndex = 0;
            for (List<String> row : data) {
                Row excelRow = sheet.createRow(rowIndex++);
                int columnIndex = 0;
                for (String column : row) {
                    if (column.length() > MAX_CELL_LENGTH) {
                        column = column.substring(0, MAX_CELL_LENGTH);
                    }
                    excelRow.createCell(columnIndex++).setCellValue(column);
                }
            }
            wb.write(out);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
