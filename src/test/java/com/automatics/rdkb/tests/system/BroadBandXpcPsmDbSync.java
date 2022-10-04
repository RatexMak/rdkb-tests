/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.system;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants.HotspotPublicInvalidParamsWEBPA;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

/**
 * Class for harvester integration with seshat
 */
public class BroadBandXpcPsmDbSync extends BroadBandWebUiBaseTest {

	/**
	 * <li>1. Get and update the 2.4GHz open public hotspot security mode param with
	 * invalid value using Webpa</li>
	 * <li>2. Verify Notification not triggered for 2.4GHz open public hotspot
	 * security mode parameter</li>
	 * <li>3. Get and update the 5GHz open public hotspot security mode param with
	 * invalid value using Webpa</li>
	 * <li>4. Verify Notification not triggered for 5GHz open public hotspot
	 * security mode parameter</li>
	 * <li>5. Get and update the 2.4GHz secure public hotspot security mode param
	 * with invalid value using Webpa</li>
	 * <li>6. Verify Notification not triggered for 2.4GHz secure public hotspot
	 * security mode parameter</li>
	 * <li>7. Get and update the 5GHz secure public hotspot security mode param with
	 * invalid value using Webpa</li>
	 * <li>8. Verify Notification not triggered for 5GHz secure public hotspot
	 * security mode parameter</li>
	 * <li>9. Get and update the 2.4GHz channel param with invalid value using
	 * Webpa</li>
	 * <li>10. Verify Notification not triggered for 2.4GHz channel parameter</li>
	 * <li>11. Get and update the 5GHz channel param with invalid value using
	 * Webpa</li>
	 * <li>12. Verify Notification not triggered for 5GHz channel parameter</li>
	 * <li>13. Get and update the 2.4GHz channel bandwidth param with invalid value
	 * using Webpa</li>
	 * <li>14. Verify Notification not triggered for 2.4GHz channel bandwidth
	 * parameter</li>
	 * <li>15. Get and update the 5GHz channel bandwidth param with invalid value
	 * using Webpa</li>
	 * <li>16. Verify Notification not triggered for 5GHz channel bandwidth
	 * parameter</li>
	 * <li>17. Get and update the 2.4GHz channel Frequency Band param with invalid
	 * value using Webpa</li>
	 * <li>18. Verify Notification not triggered for 2.4GHz channel Frequency Band
	 * parameter</li>
	 * <li>19. Get and update the 5GHz channel Frequency Band param with invalid
	 * value using Webpa</li>
	 * <li>20. Verify Notification not triggered for 5GHz channel Frequency Band
	 * parameter</li>
	 * 
	 * @author RamaTeja Meduri
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-HOTSPOT_INVALID-1001")
	public void testToVerifyXpcNotificationHotspotInvalidParams(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-HOTSPOT_INVALID-1001");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify XPC notification for public hotspot parameters with invalid values using webpa");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Get and update the 2.4GHz open public hotspot security mode param with invalid value using Webpa");
		LOGGER.info("2. Verify Notification not triggered for 2.4GHz open public hotspot security mode parameter ");
		LOGGER.info(
				"3. Get and update the 5GHz open public hotspot security mode param with invalid value using Webpa");
		LOGGER.info("4. Verify Notification not triggered for 5GHz open public hotspot security mode parameter ");
		LOGGER.info(
				"5. Get and update the 2.4GHz secure public hotspot security mode param with invalid value using Webpa");
		LOGGER.info("6. Verify Notification not triggered for 2.4GHz secure public hotspot security mode parameter ");
		LOGGER.info(
				"7. Get and update the 5GHz secure public hotspot security mode param with invalid value using Webpa");
		LOGGER.info("8. Verify Notification not triggered for 5GHz secure public hotspot security mode parameter ");
		LOGGER.info("9. Get and update the 2.4GHz channel param with invalid value using Webpa");
		LOGGER.info("10. Verify Notification not triggered for  2.4GHz channel parameter ");
		LOGGER.info("11. Get and update the 5GHz channel param with invalid value using Webpa");
		LOGGER.info("12. Verify Notification not triggered for  5GHz channel parameter ");
		LOGGER.info("13. Get and update the 2.4GHz channel bandwidth param with invalid value using Webpa");
		LOGGER.info("14. Verify Notification not triggered for  2.4GHz channel bandwidth parameter ");
		LOGGER.info("15. Get and update the 5GHz channel bandwidth param with invalid value using Webpa");
		LOGGER.info("16. Verify Notification not triggered for  5GHz channel bandwidth parameter");
		LOGGER.info("17. Get and update the 2.4GHz channel Frequency Band param with invalid value using Webpa");
		LOGGER.info("18. Verify Notification not triggered for  2.4GHz channel Frequency Band parameter ");
		LOGGER.info("19. Get and update the 5GHz channel Frequency Band param with invalid value using Webpa");
		LOGGER.info("20. Verify Notification not triggered for  5GHz channel Frequency Band parameter ");
		LOGGER.info("#######################################################################################");
		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-XPC_HOTSPOT_INVALID-101";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		boolean atomSyncStatus = CommonMethods.isAtomSyncAvailable(device, tapEnv);
		// variable declaration ends

		try {

			if (atomSyncStatus) {
				tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.COMMAND_EMPTY_WEBPA_LOG);
			} else {
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.COMMAND_EMPTY_WEBPA_LOG);
			}

			for (HotspotPublicInvalidParamsWEBPA param : BroadBandTestConstants.HotspotPublicInvalidParamsWEBPA
					.values()) {
				stepCount = verifyXpcNotificationHotspotForInvalidValues(device, testCaseId, stepCount,
						param.getWebpa(), param.getValue());
				stepCount += BroadBandTestConstants.CONSTANT_1;
			}

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying Integrate parodus2ccsp with Yocto" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			if (atomSyncStatus) {
				tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.COMMAND_EMPTY_WEBPA_LOG);
			} else {
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.COMMAND_EMPTY_WEBPA_LOG);
			}
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-HOTSPOT_INVALID-1001");
		// ###############################################################//
	}

	/**
	 * Trigger change for parameter using webpa with invalid values and validate no
	 * notification triggered in WEBPA log.
	 * 
	 * @param device         The {@link Dut} object
	 * @param testCaseId     Test case Id of the Test case called this method
	 * @param stepCount      step number of the test case
	 * @param webpaParameter Webpa parameter value to be validated in webpa log
	 * @param dataTypevalue  parameter datatype in webpa log
	 *
	 * @return step count for next step
	 * 
	 * @author RamaTeja Meduri
	 * @Refactor Sruthi Santhosh
	 */

	public int verifyXpcNotificationHotspotForInvalidValues(Dut device, String testCaseId, int stepCount,
			String webpaParameter, String dataTypevalue) {
		boolean status = false;
		String errorMessage = null;
		String currentValue = null;
		String updatedValue = null;
		String stepNumber = null;
		String webpaParam = null;
		String value = null;
		int dataType = BroadBandTestConstants.CONSTANT_0;
		webpaParam = webpaParameter;
		value = dataTypevalue;

		stepNumber = "s" + stepCount;
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get and update the " + webpaParam + " using webpa");
		LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa set command: " + webpaParam
				+ "Execute webpa set command:parameter: " + webpaParam + ", dataType: string, value: <New value>");
		LOGGER.info("STEP " + stepCount
				+ ": EXPECTED: Webpa set operation should success and value should be updated for " + webpaParam);
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get invalid response for " + webpaParam + " using webpa";
		currentValue = tapEnv.executeWebPaCommand(device, webpaParam);
		if (CommonMethods.isNotNull(currentValue)) {
			if (CommonUtils.isGivenStringAvailableInCommandOutput(webpaParam,
					BroadBandTestConstants.STRING_FOR_SEC_MODE)) {
				updatedValue = BroadBandTestConstants.STRING_VALUE_INVALID_SEC_MODE;
				dataType = BroadBandTestConstants.CONSTANT_0;
			} else if (CommonUtils.isGivenStringAvailableInCommandOutput(webpaParam,
					BroadBandTestConstants.STRING_FOR_OPERATINGBAND)) {
				updatedValue = BroadBandTestConstants.STRING_VALUE_INVALID_OPER_BAND;
				dataType = BroadBandTestConstants.CONSTANT_0;
			} else if (CommonUtils.isGivenStringAvailableInCommandOutput(webpaParam,
					BroadBandTestConstants.STRING_FOR_CHANNEL)) {
				if (CommonUtils.isGivenStringAvailableInCommandOutput(webpaParam,
						BroadBandTestConstants.STRING_FOR_BANDWIDTH)) {
					updatedValue = BroadBandTestConstants.STRING_VALUE_INVALID_OPER_BW;
					dataType = BroadBandTestConstants.CONSTANT_0;
				} else {
					updatedValue = BroadBandTestConstants.STRING_VALUE_INVALID_CHANNEL;
					dataType = BroadBandTestConstants.CONSTANT_2;
				}
			}
		}
		WebPaParameter webPaParameter = new WebPaParameter();
		webPaParameter = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(webpaParam, updatedValue, dataType);
		status = !BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
		if (status) {
			LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully got invalid respose for " + webpaParam
					+ " using webpa");
		} else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

		stepCount = verifyXpcNotificationForInvalidParamValues(device, testCaseId, stepCount, webpaParam, value,
				BroadBandTestConstants.STRING_CONSTANT_4);
		return stepCount;
	}

	/**
	 * validate log message is not present in webpa log.
	 * 
	 * @param device         The {@link Dut} object
	 * @param testCaseId     Test case Id of the Test case called this method
	 * @param stepCount      step number of the test case
	 * @param webpaParameter Webpa parameter value to be validated in webpa log
	 * @param value          parameter datatype in webpa log
	 * @param xpc            value xpc value of the P changes the value
	 *
	 * @return step count for next step
	 * 
	 * @author RamaTeja Meduri
	 * @Refactor Sruthi Santhosh
	 */

	public int verifyXpcNotificationForInvalidParamValues(Dut device, String testCaseId, int stepCount,
			String webpaParam, String value, String xpcValue) {

		boolean status = false;
		String errorMessage = null;
		String response = null;
		String logMessage = null;
		String parameter = null;
		String stepNumber = null;
		boolean atomSyncStatus = CommonMethods.isAtomSyncAvailable(device, tapEnv);

		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + stepCount;
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info(
				"STEP " + stepCount + ": DESCRIPTION: Verify payload with CMC & CID value log message in WEBPA log");
		LOGGER.info("STEP " + stepCount + ": ACTION: Execute command: "
				+ "grep -i \"Notification Event from stack\" /rdklogs/logs/WEBPAlog.txt.0 ");
		LOGGER.info("STEP " + stepCount + ": EXPECTED: Response should contain the notification in WEBPAlog file");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed as the notification event triggered for " + parameter + " in WEBPAlog.txt.0 file";
		response = atomSyncStatus
				? BroadBandCommonUtils.searchLogFilesInAtomConsoleByPolling(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_NOTIFICATION_EVENT_FROM_STACK,
						BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
				: BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_NOTIFICATION_EVENT_FROM_STACK,
						BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		parameter = DmcliUtils.convertRdkbWebPaWiFiParameterIndexToDmcliParameterIndex(webpaParam);
		logMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTraceConstants.LOG_MESSAGE_NOTIFICATION_EVENT_FROM_STACK,
				BroadBandTestConstants.COLON_AND_SPACE, BroadBandTraceConstants.LOG_MESSAGE_PARAMETER_NAME,
				BroadBandTestConstants.COLON_AND_SPACE, parameter, BroadBandTestConstants.COMMA,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTraceConstants.LOG_MESSAGE_DATATYPE,
				BroadBandTestConstants.COLON_AND_SPACE, value, BroadBandTestConstants.COMMA,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTraceConstants.LOG_MESSAGE_CHANGE_SOURCE,
				BroadBandTestConstants.COLON_AND_SPACE, xpcValue);
		status = CommonMethods.isNull(response);
		if (CommonMethods.isNotNull(response)) {
			status = !CommonMethods.patternMatcher(response, logMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified notification is not triggered for  "
					+ parameter);
		} else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//
		return stepCount;

	}

}