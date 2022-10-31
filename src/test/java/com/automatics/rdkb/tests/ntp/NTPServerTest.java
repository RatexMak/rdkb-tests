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
package com.automatics.rdkb.tests.ntp;

import java.util.Calendar;
import java.util.TimeZone;

import org.codehaus.jettison.json.JSONException;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.ntp.NTPServerUtils;

/**
 * Test class to that contains test cases which verify Moca diagnostics using
 * WEBPA for TR181 params
 * 
 * @author susheela c
 * 
 */
public class NTPServerTest extends AutomaticsTestBase {

	/**
	 * 
	 * Test case to Validate if device synchronizes times when a time drift is
	 * created and NTP server enabled
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * 
	 * <li>STEP 1: Verify if NTP enable is successful through webpa</li>
	 * <li>STEP 2: Verify if NTP server url update is successful through webpa</li>
	 * <li>STEP 3: Validate the logs for NTP server Enable, NTP server url</li>
	 * <li>STEP 4: Retrieve the current system time</li>
	 * <li>STEP 5: Introduce a time drift of 20 mins and execute date command</li>
	 * <li>STEP 6: Validate the device time after waiting for 15 mins if
	 * synchronized</li>
	 * <li>STEP 7: Verify if NTP server url change to a incorrect url is
	 * successful</li>
	 * <li>STEP 8: Check the logs to validate if the device starts polling fallback
	 * default NTP server url</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author susheela
	 * @refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.NTP })
	@TestDetails(testUID = "TC-RDKB-NTP-0003")
	public void testToVerifyIfBroadbandDeviceSynchSystemTimeWithNTPServerTime(Dut device) {

		// Test case id
		String testId = "TC-RDKB-NTP-103";
		// Test step number
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		// String to store the error message
		String errorMessage = null;
		// String to store the test case status
		boolean status = false;

		String deviceDateTime = null;

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-NTP-0003");
		LOGGER.info(
				"TEST DESCRIPTION: Validate if device syncronizes times when a time drift is created and NTP server enabled");

		LOGGER.info("STEP 1: Verify if NTP enable is successful through webpa");
		LOGGER.info("STEP 2: Verify if NTP server url update is successful through webpa");
		LOGGER.info("STEP 3: Validate the logs for NTP server Enable, NTP server url");
		LOGGER.info("STEP 4: Retrieve the current system time");
		LOGGER.info("STEP 5: Introduce a time drift of 20 mins and execute date command");
		LOGGER.info("STEP 6: Validate the device time after waiting for 15 mins if synchronized");
		LOGGER.info("STEP 7: Verify if NTP server url change to a incorrect url is successful");
		LOGGER.info("STEP 8: Check the logs to validate if the device starts polling fallback default NTP server url ");
		LOGGER.info("#######################################################################################");
		try {

			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify if NTP enable is successful through webpa");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE webpa param Device.Time.Enable to enable it");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return success response");
			LOGGER.info("**********************************************************************************");
			try {
				deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
				status = NTPServerUtils.setDeviceTimeEnableParamValue(tapEnv, device, true);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ "  : ACTUAL: Status of enabling Device.Time.Enable param through WEBPA :" + status);
			} else {
				LOGGER.error("STEP " + stepNumber
						+ " : ACTUAL: Status of enabling Device.Time.Enable param through WEBPA :" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify if NTP server url update is successful through webpa");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE webpa param to set url for Device.Time.NTPServer1");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return success response");
			LOGGER.info("**********************************************************************************");

			try {
				String ntpServerUrl = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_NTPHOST);
				status = NTPServerUtils.setDeviceTimeNTPServerParamValue(tapEnv, device, ntpServerUrl);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " ACTUAL : Status of enabling Device.Time.Enable param through WEBPA : " + status);
			} else {
				LOGGER.error("STEP " + stepNumber
						+ "ACTUAL : Status of enabling Device.Time.Enable param through WEBPA : " + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Validate the logs for NTP server Enable, NTP server url");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE command \"'grep -i \"Setting NTPServer as \" /rdklogs/logs/PAMlog.txt.0|tail -1'\"");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Should return log string for NTP Disabled and Enabled, NTPServer url from PAMlog.txt.0\"");
			LOGGER.info("**********************************************************************************");

			try {
				tapEnv.waitTill(RDKBTestConstants.ONE_MINUTE_IN_MILLIS);

				String ntpServerUrl = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_NTPHOST);
				status = NTPServerUtils.validateNTPServerEnableAndUrlInLogs(tapEnv, device, ntpServerUrl,
						deviceDateTime);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " ACTUAL: Logs validation status for ntp server enable and ntp server url:" + status);
			} else {
				LOGGER.error("STEP " + stepNumber
						+ "ACTUAL: Logs validation status for ntp server enable and ntp server url:" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Retrieve the current system time");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE command date");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return current date and time as response");
			LOGGER.info("**********************************************************************************");

			Calendar stbTime = null;
			try {
				stbTime = NTPServerUtils.getStbTime(device, tapEnv);
				status = true;
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " ACTUAL: Status of retrieving the system time :" + status);
			} else {
				LOGGER.error("STEP " + stepNumber + "ACTUAL: Status of retrieving the system time :" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Introduce a time drift of 30 mins");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE command for time drift");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : Should return date and time added with 30 mins as response");
			LOGGER.info("**********************************************************************************");

			try {
				// Update time for 20 Minutes
				stbTime.setTimeInMillis(stbTime.getTimeInMillis() + RDKBTestConstants.THIRTY_MINUTES_IN_MILLIS);
				stbTime.setTimeZone(TimeZone.getTimeZone("UTC"));
				status = NTPServerUtils.setStbTime(device, tapEnv, stbTime);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " ACTUAL:Status of setting the system time :" + status);
			} else {
				LOGGER.error("STEP " + stepNumber + "ACTUAL: Status of setting the system time :" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Validate the device time after waiting for 15 mins if synchronized");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE command date to check timedrift");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The device time should correct the drift after synchronizing with NTP server");
			LOGGER.info("**********************************************************************************");

			try {
				Calendar stbTimeBeforeWait = NTPServerUtils.getStbTime(device, tapEnv);
				tapEnv.waitTill(RDKBTestConstants.FIFTEEN_MINUTES_IN_MILLIS);
				status = NTPServerUtils.validateStbTimeAfterNTPServerSynch(device, tapEnv, stbTimeBeforeWait);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " ACTUAL:Status of Device Time synch with NTP server :" + status);
			} else {
				LOGGER.error("STEP " + stepNumber + "ACTUAL: Status of Device Time synch with NTP server :" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify if NTP server url change to a incorrect url is successful");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE command param Device.Time.Enable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return success response");
			LOGGER.info("**********************************************************************************");

			try {
				// Setting the Device.Time.Enable to false to stop NTP server synch
				NTPServerUtils.setDeviceTimeEnableParamValue(tapEnv, device, false);
				deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
				// Setting the Device.Time.Enable to true to start NTP server synch, ntp server
				// url with incorrect url
				String ntpServerInvalidUrl = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_INVALID_NTPHOST);
				if (NTPServerUtils.setDeviceTimeEnableParamValue(tapEnv, device, true)) {
					status = NTPServerUtils.setDeviceTimeNTPServerParamValue(tapEnv, device, ntpServerInvalidUrl);
				}
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " ACTUAL:Status of incorrect NTP Server url :" + status);
			} else {
				LOGGER.error("STEP " + stepNumber + "ACTUAL: Status of incorrect NTP Server url :" + status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Check the logs to validate if the device starts polling fallback default NTP server url");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE command \"'grep -i \\\"Enabling Network Time Sync\\\" /rdklogs/logs/PAMlog.txt.0|tail -1'\"");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return log string as ntp.ccp.xcal.tv");
			LOGGER.info("**********************************************************************************");

			try {
				String ntpServerInvalidUrl = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_INVALID_NTPHOST);
				tapEnv.waitTill(RDKBTestConstants.TWO_MINUTES);
				status = NTPServerUtils.validateNTPServerEnableAndUrlInLogs(tapEnv, device, ntpServerInvalidUrl,
						deviceDateTime);
			} catch (TestException exp) {
				status = false;
				errorMessage = exp.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " ACTUAL: Logs validation of failover to NTP server url when ntp server url updated with incorrect value:"
						+ status);
			} else {
				LOGGER.error("STEP " + stepNumber
						+ "ACTUAL: Logs validation of failover to NTP server url when ntp server url updated with incorrect value:"
						+ status);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			LOGGER.error("Error thrown in execution :  Reason : " + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, false,
					exception.getMessage(), true);
		} finally {
			// Setting the Device.Time.Enable to false to stop NTP server synch
			NTPServerUtils.setDeviceTimeEnableParamValue(tapEnv, device, false);
			String ntpServerUrl = AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_NTPHOST);
			// setting back the NTP server url to default value
			status = NTPServerUtils.setDeviceTimeNTPServerParamValue(tapEnv, device, ntpServerUrl);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-NTP-0003");
	}

	/**
	 * Test case to Validate if NTP Server is enabled on device through RFC
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>STEP 1: Verify the device is able to retrieve the RFC enabled parameter
	 * from Xconf</li>
	 * <li>STEP 2: Verify if the NTP is enabled via RFC after performing reboot</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author susheela
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, groups = { TestGroup.NEW_FEATURE,
			TestGroup.NTP }, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-NTP-0001")
	public void testToVerifyIfNTPServerisEnabledByRFC(Dut device) {
		// Test case id
		String testId = "TC-RDKB-NTP-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String to store the test case status
		boolean status = false;

		String deviceDateTime = null;

		LOGGER.info("STARTING TEST CASE: TC-RDKB-NTP-0001");
		LOGGER.info("TEST DESCRIPTION: Validate if NTP Server is enabled on device through RFC");
		LOGGER.info("*************************************************************************");
		LOGGER.info("STEP 1: Verify the device is able to retrieve the RFC enabled parameter from Xconf.");
		LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP enabled param in dcmrfc.log");
		LOGGER.info("STEP 2: Verify if the NTP is enabled via RFC after performing reboot.");
		LOGGER.info(
				"EXPECTED: Should reboot successfully and contain log message that NTP server enabled in dcmrfc.log");

		try {

			LOGGER.info("Setting Device Time Enable to false as pre-condition.");
			NTPServerUtils.setDeviceTimeEnableParamValue(tapEnv, device, false);
			LOGGER.info("Waiting for 1 minute.");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = NTPServerUtils.enableOrDisableNTPFeatureByRFC(tapEnv, device, true);
			if (!status) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ " : pre-condition steps failed to configure the device to enable NTP feature by RFC");
			}
			LOGGER.info("STEP 1: Verify the device is able to retrieve the NTP parameters from Xconf RFC");
			LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP enabled param in dcmrfc.log");
			errorMessage = "Errors in validating dcmrfc logs ";
			if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device))
				status = BroadBandRfcFeatureControlUtils.verifyNTPEnableOrDisableInDCMRfcLog(tapEnv, device, true,
						deviceDateTime);
			else
				status = false;
			LOGGER.info("STEP " + testStepNumber
					+ " - ACTUAL: NTP feature returned in dcmrfc.script log with enabled as value :" + status);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s2";
			status = false;
			LOGGER.info("STEP 2: Verify if the NTP is enabled via RFC after performing reboot.");
			LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP enabled param in PAMlog.txt.0.log");
			errorMessage = "Errors in validating PAMlog.txt.0 logs ";
			String ntpServerUrl = AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_NTPHOST);
			status = NTPServerUtils.validateNTPServerEnableAndUrlInLogs(tapEnv, device, ntpServerUrl, deviceDateTime);
			LOGGER.info("STEP " + testStepNumber
					+ " - ACTUAL: NTP feature returned in dcmrfc.script log with enable as value :" + status);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception exception) {
			LOGGER.error("Error thrown in execution :  Reason : " + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, false,
					exception.getMessage(), true);
		}

		finally {
			try {
				NTPServerUtils.enableOrDisableNTPFeatureByRFC(tapEnv, device, false);
				if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
					BroadBandRfcFeatureControlUtils.removeNvramOverrideForRfc(device, tapEnv);
				}
			} catch (JSONException e) {
				LOGGER.error("Error thrown while disabling the NTP feature through RFC");
			}
		}

	}

	/**
	 * Test case to Validate if NTP Server is disbaled on device through RFC
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>STEP 1: Verify the device is able to retrieve the RFC disabled parameter
	 * from Xconf</li>
	 * <li>STEP 2: Verify if the NTP is disabled via RFC after performing
	 * reboot</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author susheela
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, groups = { TestGroup.NEW_FEATURE,
			TestGroup.NTP }, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-NTP-0002")
	public void testToVerifyIfNTPServerisDisabledByRFC(Dut device) {
		// Test case id
		String testId = "TC-RDKB-NTP-102";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String to store the test case status
		boolean status = false;

		String deviceDateTime = null;

		LOGGER.info("STARTING TEST CASE: TC-RDKB-NTP-0002");
		LOGGER.info("TEST DESCRIPTION: Validate if NTP Server is disabled on device through RFC");
		LOGGER.info("*************************************************************************");
		LOGGER.info("STEP 1: Verify the device is able to retrieve the RFC disabled parameter from Xconf.");
		LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP disabled param in dcmrfc.log");
		LOGGER.info("STEP 2: Verify if the NTP is disabled via RFC after performing reboot.");
		LOGGER.info(
				"EXPECTED: Should reboot successfully and contain log message that NTP server disabled in dcmrfc.log");

		try {
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = NTPServerUtils.enableOrDisableNTPFeatureByRFC(tapEnv, device, false);
			if (!status) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ " : pre-condition steps failed to configure the device to enable NTP feature by RFC");
			}
			LOGGER.info("STEP 1: Verify the device is able to retrieve the NTP parameters from Xconf RFC");
			LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP disabled param in dcmrfc.log");
			errorMessage = "Errors in validating dcmrfc logs ";
			if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device))
				status = BroadBandRfcFeatureControlUtils.verifyNTPEnableOrDisableInDCMRfcLog(tapEnv, device, false,
						deviceDateTime);
			else
				status = false;
			LOGGER.info("STEP " + testStepNumber
					+ " - ACTUAL: NTP feature returned in dcmrfc.script log with disable as value :" + status);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s2";
			status = false;
			LOGGER.info("STEP 2: Verify if the NTP is disabled via RFC after performing reboot.");
			LOGGER.info("EXPECTED: Should contain log strings that retrieve NTP disabled param in PAMlog.txt.0.log");
			errorMessage = "Errors in validating PAMlog.txt.0 logs ";
			String ntpServerUrl = AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROPKEY_FOR_NTPHOST);
			status = NTPServerUtils.validateNTPServerDisableAndUrlInLogs(tapEnv, device, ntpServerUrl, deviceDateTime);
			LOGGER.info("STEP " + testStepNumber
					+ " - ACTUAL: NTP feature returned in dcmrfc.script log with enable as value :" + status);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception exception) {
			LOGGER.error("Error thrown in execution :  Reason : " + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, false,
					exception.getMessage(), true);
		}

		finally {
			try {
				NTPServerUtils.enableOrDisableNTPFeatureByRFC(tapEnv, device, false);
				if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
					BroadBandRfcFeatureControlUtils.removeNvramOverrideForRfc(device, tapEnv);
				}
			} catch (JSONException e) {
				LOGGER.error("Error thrown while disabling the NTP feature through RFC");
			}
		}
	}
	
	
}