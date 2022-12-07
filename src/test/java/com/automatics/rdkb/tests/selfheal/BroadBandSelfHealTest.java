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

package com.automatics.rdkb.tests.selfheal;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.selfheal.BroadBandSelfHealUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSelfHealTest extends AutomaticsTestBase {

	/** Number of pings per server for self heal */
	public static final String NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL = "3";

	/** Maximum reboot count for self heal */
	public static final String MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL = "3";

	/** String to store the value for average memory threshold value */
	public static final String AVG_MEMORY_THRESHOLD_VALUE = "100";

	/** String to store the value for resource usage compute window */
	public static final String RESOURCE_USAGE_COMPUTE_WINDOW = "15";

	/** String to store the value for average cpu threshold value */
	public static final String AVG_CPU_THRESHOLD_VALUE = "100";

	/**
	 * Verify configuration interval for Base & Aggressive self heal
	 * <ol>
	 * <li>1. Verify selfheal is enabled by default using webpa</li>
	 * <li>2. Verify non-critical selfheal interval is 15 min using webpa</li>
	 * <li>3. Update selfheal aggressive interval as 14 min using webpa</li>
	 * <li>4. Update selfheal aggressive interval as 15 min using webpa</li>
	 * <li>5. Update selfheal aggressive interval as 5 min using webpa</li>
	 * <li>6. Update non-critical selfheal interval as 5 min using webpa</li>
	 * <li>7. Update non-critical selfheal interval as 1 min using webpa</li>
	 * <li>8. Update selfheal aggressive interval as 2 min using webpa</li>
	 * <li>9. Update selfheal aggressive interval as 1 min using webpa</li>
	 * <li>10. Update selfheal aggressive interval with negative value -1 using
	 * webpa</li>
	 * <li>11. Update selfheal aggressive interval as 5 min using webpa</li>
	 * <li>12. Update non-critical selfheal interval as 15 min using webpa</li>
	 * <li>13. Update selfheal aggressive interval as 5 min using webpa</li>
	 * </ol>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Said Hisham
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-AGGRESSIVE_SELF_HEAL-1001")
	public void testVerifyAggressiveSelfHealConfig(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-AGGRESSIVE_SELF_HEAL-101";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;

		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-AGGRESSIVE_SELF_HEAL-1001");
		LOGGER.info("TEST DESCRIPTION: Verify Aggressive selfheal & base selfheal interval configuration");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify non-critical selfheal interval is 15 min using webpa");
		LOGGER.info("2. Update selfheal aggressive interval as 14 min using webpa");
		LOGGER.info("3. Update selfheal aggressive interval as 15 min using webpa");
		LOGGER.info("4. Update selfheal aggressive interval as 5 min using webpa");
		LOGGER.info("5. Update non-critical selfheal interval as 5 min using webpa");
		LOGGER.info("6. Update non-critical selfheal interval as 1 min using webpa");
		LOGGER.info("7. Update selfheal aggressive interval as 2 min using webpa");
		LOGGER.info("8. Update selfheal aggressive interval as 1 min using webpa");
		LOGGER.info("9. Update selfheal aggressive interval with negative value -1 using webpa");
		LOGGER.info("10. Update non-critical selfheal interval as 5 min using webpa");
		LOGGER.info("11. Update selfheal aggressive interval as 5 min using webpa");
		LOGGER.info("12. Update non-critical selfheal interval as 15 min using webpa");
		LOGGER.info("13. Update selfheal aggressive interval as 5 min using webpa");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Failed to verify non-critical selfheal interval as 15 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify non-critical selfheal interval is 15 min using webpa");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 15");
			LOGGER.info("STEP 1: EXPECTED : Webpa set operation should be successful and response should be 15");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_15, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully verified Base Selfheal interval as 15 min using webpa");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s2";
			errorMessage = "Failed to update Aggressive selfheal interval as 14 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Update selfheal aggressive interval as 14 min using webpa");
			LOGGER.info(
					"STEP 2: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 14");
			LOGGER.info("STEP 2: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_VALUE_14, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully updated Aggressive selfheal interval as 14 min using webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s3";
			errorMessage = "Successfully updated Aggressive selfheal interval as 15 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Update selfheal aggressive interval as 15 min using webpa");
			LOGGER.info(
					"STEP 3: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 15");
			LOGGER.info("STEP 3: EXPECTED : Webpa set operation should be failed");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_15, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successfully verified Aggressive selfheal interval is not able to set as 15 min using webpa when base selfheal interval is 15 min");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s4";
			errorMessage = "Failed to update Aggressive selfheal interval as 5 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Update selfheal aggressive interval as 5 min using webpa");
			LOGGER.info(
					"STEP 4: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 5");
			LOGGER.info("STEP 4: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully updated Aggressive selfheal interval as 5 min using webpa");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s5";
			errorMessage = "Successfully updated base selfheal interval as 5 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Update non-critical selfheal interval as 5 min using webpa");
			LOGGER.info(
					"STEP 5: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 5");
			LOGGER.info("STEP 5: EXPECTED : Webpa set operation should be failed");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully verified base selfheal interval is not able to set below the aggressive selfheal interval");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s6";
			errorMessage = "Successfully updated base selfheal interval as 1 min when Aggressive selfheal interval is configured as 5 min";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Update non-critical selfheal interval as 1 min using webpa");
			LOGGER.info(
					"STEP 6: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 1");
			LOGGER.info("STEP 6: EXPECTED : Webpa set operation should be failed");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Successfully verified base selfheal interval value below Aggressive selfheal interval");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s7";
			errorMessage = "Failed to update the aggressive selfheal interval with minimum limit of 2 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Update selfheal aggressive interval as 2 min using webpa");
			LOGGER.info(
					"STEP 7: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 2");
			LOGGER.info("STEP 7: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Successfully updated the Aggressive selfheal interval to mininum value as 2 min using webpa");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s8";
			errorMessage = "Successfully updated aggressive selfheal interval with below the minimum limit as 1 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Update selfheal aggressive interval as 1 min using webpa");
			LOGGER.info(
					"STEP 8: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 1");
			LOGGER.info("STEP 8: EXPECTED : Webpa set operation should be failed");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : Successfully verified aggressive selfheal interval is not able to set below 2 min using webpa");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s9";
			errorMessage = "Successfully updated aggressive selfheal interval with negative value as -1 using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Update selfheal aggressive interval with negative value -1 using webpa");
			LOGGER.info(
					"STEP 9: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint -1");
			LOGGER.info("STEP 9: EXPECTED : Webpa set operation should be failed");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_NEGATIVE_1, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Successfully verified aggressive selfheal interval is not able to set negative value using webpa");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s10";
			errorMessage = "Failed to update base selfheal interval as 5 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Update non-critical selfheal interval as 5 min using webpa");
			LOGGER.info(
					"STEP 10: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 5");
			LOGGER.info("STEP 10: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Successfully updated base selfheal interval as 5 min using webpa");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s11";
			errorMessage = "Successfully updated aggressive selfheal interval as 5 min when base selfheal interval is configured as 5 min";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Update selfheal aggressive interval as 5 min using webpa");
			LOGGER.info(
					"STEP 11: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 5");
			LOGGER.info(
					"STEP 11: EXPECTED : Webpa set operation should be failed because base selfheal interval is configured as 5");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Successfully verified aggressive selfheal interval is not able to set the value as 5 min when base interval is set to 5 min");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s12";
			errorMessage = "Failed to update base selfheal interval as 15 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Update non-critical selfheal interval as 15 min using webpa");
			LOGGER.info(
					"STEP 12: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 15");
			LOGGER.info("STEP 12: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_15, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Successfully updated base selfheal interval as 15 min using webpa");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s13";
			errorMessage = "Failed to update aggressive selfheal interval as 5 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Update selfheal aggressive interval as 5 min using webpa");
			LOGGER.info(
					"STEP 13: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 5");
			LOGGER.info("STEP 13: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Successfully updated aggressive selfheal interval as 5 min using webpa");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-AGGRESSIVE_SELF_HEAL-1001");
	}

	/**
	 * Test to verify self heal feature enabled status using SNMP mibs
	 * 
	 * <ol>
	 * <li>Step 1: Verify default Self heal enabled status using SNMP MIB
	 * ".1.3.6.1.4.1.17270.44.1.1.1.0"</li>
	 * <li>Step 2: Verify default numPingsPerServer value for self heal using SNMP
	 * mib ".1.3.6.1.4.1.17270.44.1.1.2.0"</li>
	 * <li>Step 3: Verify default minNumPingServer value for self heal using SNMP
	 * mib ".1.3.6.1.4.1.17270.44.1.1.3.0"</li>
	 * <li>Step 4: Verify default pingInterval value for self heal using SNMP mib
	 * ".1.3.6.1.4.1.17270.44.1.1.4.0"</li>
	 * <li>Step 5: Verify default resourceUsageComputeWindow value for self heal
	 * using SNMP mib ".1.3.6.1.4.1.17270.44.1.1.7.0"</li>
	 * <li>Step 6: Verify default avgCPUThreshold value for self heal using SNMP mib
	 * ".1.3.6.1.4.1.17270.44.1.1.8.0"</li>
	 * <li>Step 7: Verify default avgMemoryThreshold value for self heal using SNMP
	 * mib ".1.3.6.1.4.1.17270.44.1.1.9.0"</li>
	 * <li>Step 8: Verify default maxRebootCount value for self heal using SNMP mib
	 * ".1.3.6.1.4.1.17270.44.1.1.10.0"</li>
	 * <li>Step 9: Verify default maxSubsystemResetCount value for self heal using
	 * SNMP mib ".1.3.6.1.4.1.17270.44.1.1.11.0"</li>
	 * <li>Step 10: Configure the maxSubsystemResetCount to any value other than
	 * default using SNMP MIB (.1.3.6.1.4.1.17270.44.1.1.11.0)"</li>
	 * <li>Step 11: Verify updated maxSubsytemResetCount using SNMP MIB (
	 * .1.3.6.1.4.1.17270.44.1.1.11.0)"</li>
	 * <li>Step 12: Verify updated maxSubsytemResetCount using WebPA param
	 * "Device.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount"</li>
	 * <li>Step 13: Reboot the device and verify whether device comes up
	 * properly"</li>
	 * <li>Step 14: Repeat step1 to step 9</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Gnanaprakasham S
	 * @Refactor Athira
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.SYSTEM, BroadBandTestGroup.SELF_HEAL })
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4000")

	public void testVerifySelfHealingFeatureEnabledByDefault(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-400";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// Hash map to store self heal configuration
		HashMap<String, String> selfHealConfiguration = new HashMap<>();
		// integer to store precondition step number
		String response = null;
		int preConStepNumber = 1;
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SELF-HEAL-4000");
			LOGGER.info(
					"TEST DESCRIPTION: Test to Verify default Self Heal configuration and configuration persistence after reboot");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"STEP 1: Verify default Self heal enabled status using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.1.0\"");
			LOGGER.info(
					"STEP 2:Verify default numPingsPerServer value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.2.0\"");
			LOGGER.info(
					"STEP 3: Verify default minNumPingServer value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.3.0\"");
			LOGGER.info(
					"STEP 4: Verify default pingInterval value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.4.0\"");
			LOGGER.info(
					"STEP 5: Verify default resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info(
					"STEP 6:Verify default avgCPUThreshold value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
			LOGGER.info(
					"STEP 7: Verify default avgMemoryThreshold value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
			LOGGER.info(
					"STEP 8: Verify default maxRebootCount value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.10.0\"");
			LOGGER.info(
					"STEP 9: Verify default maxSubsystemResetCount value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.11.0\" ");
			LOGGER.info(
					"STEP 10:Configure the maxSubsystemResetCount to any value other than default using SNMP MIB (.1.3.6.1.4.1.17270.44.1.1.11.0)");
			LOGGER.info(
					"STEP 11:Verify updated maxSubsytemResetCount using SNMP MIB ( .1.3.6.1.4.1.17270.44.1.1.11.0) ");
			LOGGER.info(
					"STEP 12: Verify updated maxSubsytemResetCount using WebPA param \"Device.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount\"");
			LOGGER.info("STEP 13: Reboot the device and verify whether device comes up properly");
			LOGGER.info(
					"STEP 14: Verify Self heal enabled status after reboot using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.1.0\"");
			LOGGER.info(
					"STEP 15: Verify numPingsPerServer value after reboot for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.2.0\"");
			LOGGER.info(
					"STEP 16:Verify minNumPingServer value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.3.0\"");
			LOGGER.info(
					"STEP 17: Verify pingInterval value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.4.0\"");
			LOGGER.info(
					"STEP 18: Verify resourceUsageComputeWindow value for self heal V using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info(
					"STEP 19: Verify avgCPUThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\" ");
			LOGGER.info(
					"STEP 20: Verify avgMemoryThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
			LOGGER.info(
					"STEP 21: Verify maxRebootCount value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.10.0\" ");
			LOGGER.info(
					"STEP 22: Verify maxSubsystemResetCount value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.11.0\"");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv, preConStepNumber);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			errorMessage = "Failed to get default self heal enabled status as 1. Expected status should be 1 but Actual obtained value is :"
					+ selfHealConfiguration.get(BroadBandTestConstants.STRING_SELF_HEAL_ENABLED_STATUS);
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1:DESCRIPTION:Verify default Self heal enabled status using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.1.0\"");
			LOGGER.info(
					"STEP 1:ACTION: Execute SNMP MIB (.1.3.6.1.4.1.17270.44.1.1) and retrieve default configuration");
			LOGGER.info("STEP 1:EXPECTED: Default Self Heal enabled status should be 1");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELFHEAL_PROCESS_ENABLE_STATUS);
					LOGGER.info("Self Heal Enabled status retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 1: ACTUAL : SUCCESSFULLY OBTAINED SELF HEAL ENABLED STATUS AS TRUE USING WEBPA");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);
					status = selfHealConfiguration.get(BroadBandTestConstants.STRING_SELF_HEAL_ENABLED_STATUS)
							.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 1: ACTUAL : SUCCESSFULLY OBTAINED SELF HEAL ENABLED STATUS AS 1 USING SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.1.0\"");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			}

			testStepNumber = "s2";
			errorMessage = "Not able to get number of pings per server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.2.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 2:DESCRIPTION: Verify default numPingsPerServer value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.2.0\"");
			LOGGER.info(
					"STEP 2:ACTION: Execute command snmpwalk by using SNMP oid .1.3.6.1.4.1.17270.44.1.1.2.0 then verify response");
			LOGGER.info("STEP 2:EXPECTED: Default numPingsPerServer value for self heal should be 3");
			LOGGER.info(
					"************************************************************************************************");

			try {
				// verify number of pings per server for self heal configuration
				// number of pings should be 3
				status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
						BroadBandTestConstants.STRING_NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL,
						NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL);
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : SUCCESSFULLY OBTAINED the number of pings per server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.2.0\"");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s3";
			errorMessage = "Not able to get minimum number of ping server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.3.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 3:DESCRIPTION: Verify default minNumPingServer value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.3.0\"");
			LOGGER.info(
					"STEP 3:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.3.0 then verify response");
			LOGGER.info("STEP 3:EXPECTED: Default minNumPingServer value for self heal should be 1 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
						BroadBandTestConstants.SELF_HEAL_MINIMUM_NUMBER_OF_PING_SERVER,
						BroadBandTestConstants.STRING_VALUE_ONE);
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : SUCCESSFULLY OBTAINED minimum number of ping server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.3.0\" ");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s4";
			errorMessage = "Not able to get ping interval details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.4.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION: Verify default pingInterval value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.4.0\"");
			LOGGER.info(
					"STEP 4:ACTION: Execute command snmpwalk by using SNMP oid .1.3.6.1.4.1.17270.44.1.1.4.0 then verify response");
			LOGGER.info("STEP 4:EXPECTED: Default pingInterval value for self heal should be 60");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
						BroadBandTestConstants.STRING_PING_INTERVAL_FOR_SELF_HEAL,
						BroadBandTestConstants.SELF_HEAL_PING_INTERVAL);
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : SUCCESSFULLY OBTAINED ping interval details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.4.0\" ");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s5";
			errorMessage = "Not able to get resourceUsageComputeWindow details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.7.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 5:DESCRIPTION: Verify default resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info(
					"STEP 5:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.7.0 then verify response");
			LOGGER.info("STEP 5:EXPECTED: Default resourceUsageComputeWindow value for self heal should be 15 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
						BroadBandTestConstants.STRING_RESOURCE_USAGE_FOR_SELF_HEAL, BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty());
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : SUCCESSFULLY OBTAINED get resourceUsageComputeWindow details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.7.0\" ");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s6";
			errorMessage = "Not able to get avgCPUThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.8.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 6:DESCRIPTION:Verify default avgCPUThreshold value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
			LOGGER.info(
					"STEP 6:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.8.0 then verify response");
			LOGGER.info("STEP 6:EXPECTED: Default avgCPUThreshold value for self heal should be 100 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
						BroadBandTestConstants.STRING_AVG_CPU_THRESHOLD_FOR_SELF_HEAL, AVG_CPU_THRESHOLD_VALUE);
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : SUCCESSFULLY OBTAINED to get the avgCPUThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s7";
			errorMessage = "Not able to get avgMemoryThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.9.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 7:DESCRIPTION: Verify default avgMemoryThreshold value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
			LOGGER.info(
					"STEP 7:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.9.0 then verify response");
			LOGGER.info("STEP 7:EXPECTED: Default avgMemoryThreshold value for self heal should be 100 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD);
					LOGGER.info("Average Memory threshold retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AVG_MEMORY_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL : SUCCESSFULLY OBTAINED get avgMemory Threshold details for self heal configuration using WEBPA");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_AVG_MEMORY_THRESHOLD_FOR_SELF_HEAL,
							AVG_MEMORY_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL : SUCCESSFULLY OBTAINED to get avgMemoryThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s8";
			errorMessage = "Not able to get maxRebootCount details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.10.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 8:DESCRIPTION: Verify default maxRebootCount value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.10.0\"");
			LOGGER.info(
					"STEP 8:ACTION: Execute snmpwalk using mib .1.3.6.1.4.1.17270.44.1.1.10.0 then verify response");
			LOGGER.info("STEP 8:EXPECTED: Default maxRebootCount value for self heal should be 3");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_REBOOT_COUNT);
					LOGGER.info("maxRebootCount retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 8: ACTUAL :SUCCESSFULLY OBTAINED the Default maxRebootCount value for self heal should be 3 using WEBPA");
				} else {
					LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL,
							MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 8: ACTUAL : SUCCESSFULLY OBTAINED the Default maxRebootCount value for self heal should be 3");
				} else {
					LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s9";
			String expectedValue = BroadBandTestConstants.STRING_VALUE_THREE;
			errorMessage = "Not able to get maxSubsystemResetCount details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.11.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 9:DESCRIPTION: Verify default maxSubsystemResetCount value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.11.0\" ");
			LOGGER.info(
					"STEP 9:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.11.0 then verify response");
			LOGGER.info(
					"STEP 9:EXPECTED: Default maxSubsystemResetCount value for self heal should be 3 or RFC set value for parameter-maxSubsystemResetCount");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_RESET_COUNT);
					LOGGER.info("maxSubsystemResetCount retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 9: ACTUAL :SUCCESSFULLY OBTAINED the Default maxSubsystemResetCount value for self heal should be 3 using WEBPA");
				} else {
					LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {

					try {

						String deviceStatus = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
								device, BroadBandTestConstants.DEVICE_STATUS);
						if (Boolean.parseBoolean(deviceStatus)) {
							status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
									BroadBandTestConstants.STRING_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL,
									BroadBandTestConstants.STRING_VALUE_THREE);
							if (!status) {
								response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
										BroadBandTestConstants.TEXT_DOUBLE_QUOTE
												+ BroadBandWebPaConstants.WEBPA_PARAM_MAX_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL
												+ BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
										BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG,
										BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
								LOGGER.info("RESPONSE IS" + response);
								LOGGER.info("maxResetCountValue From Rfc Is-" + CommonMethods.patternFinder(response,
										BroadBandTestConstants.PATTERN_TO_GET_MAX_RESET_COUNT));
								status = CommonMethods.isNotNull(response)
										&& BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
												BroadBandTestConstants.STRING_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL,
												CommonMethods.patternFinder(response,
														BroadBandTestConstants.PATTERN_TO_GET_MAX_RESET_COUNT));
								if (status) {
									expectedValue = CommonMethods.patternFinder(response,
											BroadBandTestConstants.PATTERN_TO_GET_MAX_RESET_COUNT);
								}
							}
						}
					} catch (Exception e) {
						status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
								BroadBandTestConstants.STRING_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL,
								BroadBandTestConstants.STRING_VALUE_THREE);
						LOGGER.info("Device specific Status found");
					}

				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 9: ACTUAL : SUCCESSFULLY OBTAINED the Default maxSubsystemResetCount for self heal should be"
									+ expectedValue);
				} else {
					LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}
			testStepNumber = "s10";
			errorMessage = "Not able to set maxSubsystemResetCount details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.11.0\" ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 10:DESCRIPTION:Configure the maxSubsystemResetCount to any value other than default using SNMP MIB (.1.3.6.1.4.1.17270.44.1.1.11.0)");
			LOGGER.info("STEP 10:ACTION: Execute command snmpset using OID .1.3.6.1.4.1.17270.44.1.1.11.0 i 10");
			LOGGER.info("STEP 10:EXPECTED: SNMP configurations must be successful");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.MAX_RESET_COUNT, BroadBandTestConstants.CONSTANT_2,
							BroadBandTestConstants.STRING_VALUE_TEN);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 10: ACTUAL : SUCCESSFULLY set maxSubsystemResetCount value as 10 using WebPA");
				} else {
					LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
						BroadBandSnmpMib.ECM_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL.getOid(),
						SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_TEN);
				LOGGER.info("waiting for 2 minutes");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TEN);
				if (status) {
					LOGGER.info("STEP 10: ACTUAL : SUCCESSFULLY set maxSubsystemResetCount value as 10");
				} else {
					LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s11";
			errorMessage = "Failed to get maxSubsystemResetCount value as 10. Expected value should be 10 But Actual Obtained value : "
					+ response;
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 11 :DESCRIPTION:Verify updated maxSubsytemResetCount using SNMP MIB ( .1.3.6.1.4.1.17270.44.1.1.11.0) ");
			LOGGER.info(
					"STEP 11:ACTION: Execute command snmpget using OID .1.3.6.1.4.1.17270.44.1.1.11.0 then verify response");
			LOGGER.info(
					"STEP 11:EXPECTED: maxSubsytemResetCount configurations must be retrieved successfully and should be same as step 10");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_RESET_COUNT);
					LOGGER.info("maxSubsystemResetCount retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TEN);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 11: ACTUAL :SUCCESSFULLY retrieved maxSubsystemResetCount  and the value is 10 using Webpa");
				} else {
					LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL.getOid());
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TEN);
				if (status) {
					LOGGER.info("STEP 11: ACTUAL : SUCCESSFULLY retrieved maxSubsystemResetCount  and the value is 10");
				} else {
					LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s12";
			errorMessage = "Failed to get Maximum sub system reset count as 10 using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount\". Expected value should be 10. Biut actual Obatined value is :"
					+ response;
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 12:DESCRIPTION: Verify updated maxSubsytemResetCount using WebPA param \"Device.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount\"");
			LOGGER.info(
					"STEP 12:ACTION: Execute command to update maxSubsytemResetCount then verify response");
			LOGGER.info(
					"STEP 12:EXPECTED: maxSubsytemResetCount configurations must be retrieved successfully and should be same as step 10");
			LOGGER.info(
					"************************************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_MAX_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TEN);
			if (status) {
				LOGGER.info(
						"STEP 12: ACTUAL : SUCCESSFULLY retrieved maxSubsystemResetCount  and the value is 10 by WebPA after Reboot");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s13";
			errorMessage = "Failed to reboot the device or device not reachable after reboot";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION: Reboot the device and verify whether device comes up properly");
			LOGGER.info("STEP 13: ACTION: Execute command: /sbin/reboot");
			LOGGER.info("STEP 13: EXPECTED: Device should be reachable after reboot");
			LOGGER.info("******************************************************************************");

			status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL: Successfully rebooted the device and able to SSH after reboot");
			} else {
				LOGGER.error("STEP 13: ACTUAL: " + errorMessage);
			}
			LOGGER.info("********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s14";
			errorMessage = "Failed to get self heal enabled status as 1 after reboot. Expected status should be 1 but Actual obtained value is :"
					+ selfHealConfiguration.get(BroadBandTestConstants.STRING_SELF_HEAL_ENABLED_STATUS);
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 14:DESCRIPTION: Verify Self heal enabled status after reboot using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.1.0\"");
			LOGGER.info(
					"STEP 14:ACTION: Execute SNMP MIB (.1.3.6.1.4.1.17270.44.1.1) and retrieve default configuration");
			LOGGER.info("STEP 14:EXPECTED: Self Heal Configurations enabled status should be 1 after reboot");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					long startTime = System.currentTimeMillis();
					do {
						response = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELFHEAL_PROCESS_ENABLE_STATUS);
						LOGGER.info("Self Heal Enabled status retrieved using WebPa = " + response);
						status = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
					} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTES
							&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
							&& !status);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 14: ACTUAL : SUCCESSFULLY OBTAINED SELF HEAL ENABLED STATUS AS TRUE USING WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					long startTime = System.currentTimeMillis();
					status = false;
					do {
						selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);
						if (selfHealConfiguration != null && selfHealConfiguration.size() != 0) {
							status = selfHealConfiguration.get(BroadBandTestConstants.STRING_SELF_HEAL_ENABLED_STATUS)
									.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
						}
						if (status) {
							break;
						}
					} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTES
							&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 14: ACTUAL: Self Heal Configurations enabled status is 1 after reboot");
				} else {
					LOGGER.error("STEP 14: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			}

			testStepNumber = "s15";
			errorMessage = "Not able to get number of pings per server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.2.0\" after STB reboot ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 15:DESCRIPTION: Verify numPingsPerServer value after reboot for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.2.0\"");
			LOGGER.info(
					"STEP 15:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.2.0 then verify response");
			LOGGER.info("STEP 15:EXPECTED: numPingsPerServer value for self heal should be 3 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.NUM_PINGS_PER_SERVER);
					LOGGER.info("Num Ping per server retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 15: ACTUAL : SUCCESSFULLY OBTAINED the number of pings per server details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL,
							NUMBER_PINGS_PER_SERVER_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 15: ACTUAL: numPingsPerServer value for self heal is 3 after reboot");
				} else {
					LOGGER.error("STEP 15: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s16";
			errorMessage = "Not able to get minimum number of ping server details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.3.0\" after STB reboot";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 16:DESCRIPTION:Verify minNumPingServer value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.3.0\"");
			LOGGER.info(
					"STEP 16:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.3.0 then verify response");
			LOGGER.info("STEP 16:EXPECTED: minNumPingServer value for self heal should be 1 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MIN_NUM_PINGS_SERVER);
					LOGGER.info("Min Num Ping per server retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 16: ACTUAL : SUCCESSFULLY OBTAINED the minimum number of pings per server details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.SELF_HEAL_MINIMUM_NUMBER_OF_PING_SERVER,
							BroadBandTestConstants.STRING_VALUE_ONE);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 16: ACTUAL: minNumPingServer value for self heal is 1 after reboot");
				} else {
					LOGGER.error("STEP 16: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s17";
			errorMessage = "Not able to get ping interval details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.4.0\" after STB reboot";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 17:DESCRIPTION: Verify pingInterval value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.4.0\"");
			LOGGER.info(
					"STEP 17:ACTION: Execute command snmpwalk using oid .1.3.6.1.4.1.17270.44.1.1.4.0 then verify response");
			LOGGER.info("STEP 17:EXPECTED:  pingInterval value for self heal should be 60 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
					LOGGER.info("Ping Interval Value retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.SELF_HEAL_PING_INTERVAL);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 17: ACTUAL : SUCCESSFULLY OBTAINED the ping interval details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_PING_INTERVAL_FOR_SELF_HEAL,
							BroadBandTestConstants.SELF_HEAL_PING_INTERVAL);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 17: ACTUAL: pingInterval value for self heal is 60 after reboot");
				} else {
					LOGGER.error("STEP 17: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s18";
			errorMessage = "Not able to get resourceUsageComputeWindow details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.7.0\" after STB reboot";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 18:DESCRIPTION: Verify resourceUsageComputeWindow value for self heal V using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info(
					"STEP 18:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.7.0 then verify response");
			LOGGER.info("STEP 18:EXPECTED: resourceUsageComputeWindow value for self heal should be 15 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
					LOGGER.info("Resource usage compute window retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(RESOURCE_USAGE_COMPUTE_WINDOW);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 18: ACTUAL : SUCCESSFULLY OBTAINED get resourceUsageComputeWindow details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_RESOURCE_USAGE_FOR_SELF_HEAL, RESOURCE_USAGE_COMPUTE_WINDOW);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 18: ACTUAL: resourceUsageComputeWindow value for self heal is 15 after reboot");
				} else {
					LOGGER.error("STEP 18: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s19";
			errorMessage = "Not able to get avgCPUThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.8.0\" after STB reboot ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 19:DESCRIPTION: Verify avgCPUThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\" ");
			LOGGER.info(
					"STEP 19:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.8.0 then verify response");
			LOGGER.info("STEP 19:EXPECTED:  avgCPUThreshold value for self heal should be 100 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_CPU_THRESHOLD);
					LOGGER.info("Average CPU threshold retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AVG_CPU_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 19: ACTUAL : SUCCESSFULLY OBTAINED get avgCPUThreshold details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_AVG_CPU_THRESHOLD_FOR_SELF_HEAL, AVG_CPU_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 19: ACTUAL: avgCPUThreshold value for self heal is 100 after reboot");
				} else {
					LOGGER.error("STEP 19: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s20";
			errorMessage = "Not able to get avgMemoryThreshold details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.9.0\" after STB reboot";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 20:DESCRIPTION: Verify avgMemoryThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
			LOGGER.info(
					"STEP 20:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.9.0 then verify response");
			LOGGER.info("STEP 20:EXPECTED: avgMemoryThreshold value for self heal should be 100 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD);
					LOGGER.info("Average Memory threshold retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AVG_MEMORY_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 20: ACTUAL : SUCCESSFULLY OBTAINED get avgMemory Threshold details for self heal configuration using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_AVG_MEMORY_THRESHOLD_FOR_SELF_HEAL,
							AVG_MEMORY_THRESHOLD_VALUE);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 20: ACTUAL: avgMemoryThreshold value for self heal is 100 after reboot");
				} else {
					LOGGER.error("STEP 20: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s21";
			errorMessage = "Not able to get maxRebootCount details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.10.0\" after STB reboot ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 21:DESCRIPTION: Verify maxRebootCount value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.10.0\" ");
			LOGGER.info(
					"STEP 21:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.10.0 then verify response");
			LOGGER.info("STEP 21:EXPECTED: maxRebootCount value for self heal should be 3");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_REBOOT_COUNT);
					LOGGER.info("maxRebootCount retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 21: ACTUAL :SUCCESSFULLY OBTAINED the Default maxRebootCount value for self heal should be 3 using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {
					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL,
							MAXIMUM_REBOOT_COUNT_FOR_SELF_HEAL);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 21: ACTUAL: maxRebootCount value for self heal is 3 after reboot");
				} else {
					LOGGER.error("STEP 21: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s22";
			errorMessage = "Not able to get maxSubsystemResetCount details for self heal configuration using SNMP MIB \".1.3.6.1.4.1.17270.44.1.1.11.0\" after STB reboot";
			status = false;
			expectedValue = BroadBandTestConstants.STRING_VALUE_TEN;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 22:DESCRIPTION: Verify maxSubsystemResetCount value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.11.0\"");
			LOGGER.info(
					"STEP 22:ACTION: Execute command snmpwalk using OID .1.3.6.1.4.1.17270.44.1.1.11.0 then verify response");
			LOGGER.info(
					"STEP 22:EXPECTED:  maxSubsystemResetCount value for self heal should be 10 or should have RFC set value for tr181.Device.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_RESET_COUNT);
					LOGGER.info("maxSubsystemResetCount retrieved using WebPa = " + response);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TEN);
				} catch (Exception exception) {
					errorMessage = "Exception occured during execution : " + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 22: ACTUAL :SUCCESSFULLY OBTAINED the Default maxSubsystemResetCount value for self heal should be 10 using WEBPA after Reboot");
				} else {
					LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				try {

					status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
							BroadBandTestConstants.STRING_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL,
							BroadBandTestConstants.STRING_VALUE_TEN);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 22: ACTUAL: maxSubsystemResetCount value for self heal is-" + expectedValue
							+ "after Reboot");
				} else {
					LOGGER.error("STEP 22: ACTUAL: " + errorMessage);
				}
				LOGGER.info("********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

		} catch (Exception exception) {
			errorMessage = "Exception occured during execution :" + exception.getMessage();
			LOGGER.error(errorMessage);
		} finally {
			if (DeviceModeHandler.isDSLDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.MAX_RESET_COUNT, BroadBandTestConstants.CONSTANT_2,
						BroadBandTestConstants.STRING_VALUE_THREE);
				if (status) {
					LOGGER.info(
							"POST-CONDITION 1  : ACTUAL: SUCCESSFULLY set maxSubsystemResetCount value as 3 using WebPA.");
				} else {
					LOGGER.error("POST-CONDITION 1: ACTUAL: " + errorMessage);
				}
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_2);
			} else {
				// as a post condition setting maxSubsystemResetCount value to
				// default value (3)
				BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
						BroadBandSnmpMib.ECM_MAXIMUM_SUB_SYSTEM_RESET_COUNT_FOR_SELF_HEAL.getOid(),
						SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_THREE);
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_1);
			}

		}

	}

	/**
	 * Test to Validate if the cron scheduling on ATOM side whether it is having
	 * LogUploadFrequency value _TR181
	 *
	 * <ol>
	 * <p>
	 * <li>STEP 1 :Enable diagnostic mode using webpa params</li>
	 * <li>STEP 2 :verify logupload frequency using webpa params</li>
	 * <li>STEP 3 :Configure log upload frequency using webpa params</li>
	 * <li>STEP 4 :verify logupload frequency using webpa params</li>
	 * <li>STEP 5 :Disable diagnostic mode using webpa params</li>
	 * <li>STEP 6 :verify default value logupload frequency using webpa params</li>
	 * </p>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Gnanaprakasham S
	 * @Refactor Sruthi Santhosh
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.SYSTEM, BroadBandTestGroup.SELF_HEAL })
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4003")
	public void testVerifyLogUploadFrequencyForCronSchedulingOnAtomSideUsingWebpa(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-403";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String variable to store command response
		String response = null;
		// String variable to store log upload frequency
		String logUploadFrequency = null;

		try {

			LOGGER.info("STARTING TEST CASE: " + testId);

			LOGGER.info("**************************************************************");
			LOGGER.info(
					"TEST DESCRIPTION: Test to validate if the cron scheduling on ATOM side whether it is having LogUploadFrequency value _TR181");
			LOGGER.info("*************************************************************************");

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1: Enable the diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ");
			LOGGER.info("EXPECTED: Diagnostic mode must be enabled via Tr181 dataobject ");
			LOGGER.info(
					"STEP 2: Verify Logupload frequency value webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \" ");
			LOGGER.info("EXPECTED: Response should contains valid integer value for Logupload frequency");
			LOGGER.info(
					"STEP 3:Configure log upload frequency as 60 using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \"");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"STEP 4: Verify log upload frequency value using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \" ");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"STEP 5: Disable diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" and verify disabled status ");
			LOGGER.info("EXPECTED: Diagnostic mode must be disabled via webpa param ");
			LOGGER.info(
					"STEP 6: Verify log upload frequency value using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \"");
			LOGGER.info("EXPECTED: Log upload frequency value must be 1440 ");

			LOGGER.info(
					"************************************************************************************************");

			if (!CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, true);
			}

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1: Enable the diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" and verify diagnostic mode");
			LOGGER.info("STEP 1: EXPECTED: Diagnostic mode must be enabled via Tr181 dataobject ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to enable diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ";
				status = BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
						BroadBandTestConstants.TRUE);
				LOGGER.info("Diagnostic mode enabled status via webpa : " + status);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : SUCCESSFULLY ENABLED DIAGNOSTIC MODE UISNG WEBPA PARAMETER !!!");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s2";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 2: Verify Logupload frequency value webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \" ");
			LOGGER.info(
					"STEP 2: ACTION: Execute webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \"and verify log upload frequency");
			LOGGER.info("STEP 2: EXPECTED: Response should contains valid integer value for Logupload frequency");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to verify Logupload frequency value webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \"";
				logUploadFrequency = CommonMethods.executeWebPA(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY);
				LOGGER.info("Obtained respobnse for log upload frequecy : " + logUploadFrequency);
				status = CommonMethods.isNotNull(logUploadFrequency);

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : SUCCESSFULLY VERIFIED LOG UPLOAD FREQUENCY UISNG WEBPA PARAMETER !!!");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s3";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 3:Configure log upload frequency as 60 using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\"");
			LOGGER.info(
					"STEP 3: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" and set log upload frequency as 60");
			LOGGER.info("STEP 3: EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to configure log upload frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \" ";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY,
						WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.STRING_VALUE_60);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : SUCCESSFULLY CONFIGURED LOG UPLOAD FREQUENCY USING WEBPA PARAM Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency!!!");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s4";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 4: Verify log upload frequency value using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" ");
			LOGGER.info(
					"STEP 4: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" and verify log upload frequency");
			LOGGER.info("STEP 4: EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				errorMessage = "Failed to get the updtated log uplod frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency \" ";
				response = CommonMethods.executeWebPA(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY);

				if (CommonMethods.isNotNull(response)) {
					LOGGER.info("SUccessfully obtained log upload frequency using webpa parameter  : " + response);
					status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_60);
				}

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : SUCCESSFULLY VERIFIED CONFIGURED LOG UPLOAD FREQUENCY USING webpa params !!!");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s5";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 7: Disable diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ");
			LOGGER.info(
					"STEP 7: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" and verify diagnostic mode status");
			LOGGER.info("STEP 5: EXPECTED: Diagnostic mode must be disabled via webpa params ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to disable diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ";
				status = BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
						BroadBandTestConstants.FALSE);
				LOGGER.info("Diagnostic mode disabled status via webpa : " + status);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : SUCCESSFULLY DISABLED DIAGNOSTIC MODE USING WEBPA PARAMS !!!");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s6";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 6: Verify log upload frequency value using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\"");
			LOGGER.info(
					"STEP 6: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" and verify log upload frequency");
			LOGGER.info("STEP 6: EXPECTED: Log upload frequency value must be retained and value should be 60 ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				errorMessage = "Failed to retain the value for log uplod frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" ";
				response = CommonMethods.executeWebPA(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY);

				if (CommonMethods.isNotNull(response)) {
					LOGGER.info("Successfully retained the value for log upload frequency using webpa parameter  : "
							+ response);
					status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_60);
				}

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : SUCCESSFULLY RETAINED VALUE FOR LOG UPLOAD FREQUENCY USING SNMP MIB !!!");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = "Exception occurred during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} finally {
			BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
					BroadBandTestConstants.FALSE);
			BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY,
					WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.STRING_VALUE_1440);
		}
		LOGGER.info("ENDING TEST CASE: " + testId);
	}

	/**
	 * Verify wan link heal check after firmware upgrade
	 * <ol>
	 * <li>Verify else set Wan Link Heal enable with value true</li>
	 * <li>Perform CDL to latest build on device</li>
	 * <li>Verify wan link heal is invoked after 15 mins of uptime</li>
	 * <li>Verify cm status is obtained and logged</li>
	 * <li>Verify cm ip is obtained and logged with correct values</li>
	 * <li>Verify wan ip is obtained and logged with correct values</li>
	 * <li>Verify ping ipv4 status is logged</li>
	 * <li>Verify device compares current and previous wan status to determine
	 * reboot needed</li>
	 * <li>Set value of Wan Link Heal enable to false</li>
	 * <li>Perform CDL to original build on device</li>
	 * <li>Verify value of Wan Link Heal enable persists after CDL</li>
	 * <li>Verify Wan Link Heal is not invoked after CDL when it is disabled</li>
	 * </ol>
	 * 
	 * @author Ashwin Sankara
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4011")
	public void testVerifySelfHealRebootAfterUpgrade(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SELF-HEAL-411";
		String stepNum = "s1";
		String errorMessage = null;
		String latestImage = null;
		String currentImage = null;
		String paramIp = null;
		boolean status = false;
		boolean hasOriginalBuildChanged = false;
		boolean hasLatestBuildChanged = false;
		currentImage = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SELF-HEAL-4011");
		LOGGER.info("TEST DESCRIPTION: Verify wan link heal check after firmware upgrade");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify else set Wan Link Heal enable with value true");
		LOGGER.info("2. Perform CDL to latest build on device");
		LOGGER.info("3. Verify wan link heal is invoked after 15 mins of uptime");
		LOGGER.info("4. Verify cm status is obtained and logged");
		LOGGER.info("5. Verify cm ip is obtained and logged with correct values");
		LOGGER.info("6. Verify wan ip is obtained and logged with correct values");
		LOGGER.info("7. Verify ping ipv4 status is logged");
		LOGGER.info("8. Verify device compares current and previous wan status to determine reboot needed");
		LOGGER.info("9. Set value of Wan Link Heal enable to false");
		LOGGER.info("10. Perform CDL to original build on device");
		LOGGER.info("11. Verify value of Wan Link Heal enable persists after CDL");
		LOGGER.info("12. Verify Wan Link Heal is not invoked after CDL when it is disabled");

		LOGGER.info("#######################################################################################");

		try {

			errorMessage = "Failed to set value of Wan Link Heal enable to true";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify else set Wan Link Heal enable with value true");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WANLinkHeal.Enable");
			LOGGER.info("STEP 1: EXPECTED : Value of Wan Link Heal enable parameter is true");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_LINK_HEAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Value of Wan Link Heal enable parameter is true");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s2";
			errorMessage = "Failed to get current image from the device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Perform CDL to latest build on device");
			LOGGER.info(
					"STEP 2: ACTION : 1. Get latest build from RDK portal\n2. Configure mock server with latest stable image for estb mac of the device\n3. Set xconf check-in TR-181 parameter\n4. Wait for device to reboot and come up with new image");
			LOGGER.info("STEP 2: EXPECTED : Upgraded device to latest build successfully");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isNotNull(currentImage)) {
				errorMessage = "Failed to get latest image from rdkportal for device model: " + device.getModel();
				latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
				LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
				if (CommonMethods.isNull(latestImage)) {
					LOGGER.info(
							" GA image obtained from deployed version service is null. Hence getting the image from property file ");
					latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
					LOGGER.info("Latest Firmware version from property file: " + latestImage);
				}
				if (CommonMethods.isNotNull(latestImage)) {
					errorMessage = "Unable to trigger CDL for latest build - " + latestImage;
					status = FirmwareDownloadUtils.performXconfHttpImageUpgrade(tapEnv, device, latestImage);
				}
			}

			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info("STEP 2: ACTUAL : Upgraded device to latest build successfully");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s3";
			errorMessage = "Failed to find Wan Link Heal invoked log message in SelfHeal.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify wan link heal is invoked after 15 mins of uptime");
			LOGGER.info(
					"STEP 3: ACTION : Execute command: grep \"Wan Link Heal for bootup-check invoked\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 3: EXPECTED : Wan link heal invoked log message is present");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_WAN_LINK_HEAL_INVOKED,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Wan link heal invoked log message is present");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s4";
			errorMessage = "Failed to find cm_status=OPERATIONAL log message in SelfHeal.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify cm status is obtained and logged");
			LOGGER.info(
					"STEP 4: ACTION : Execute command: grep \"cm_status=OPERATIONAL\" /rdklogs/logs/SelfHeal.txt.0 \ngrep \"cm_status=1\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 4: EXPECTED : Cm status is obtained and logged");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CM_STATUS_OPERATIONAL,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
				errorMessage = "Failed to find cm_status=1 log message in SelfHeal.txt.0";
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_CM_STATUS_ONE, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Cm status is obtained and logged");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s5";
			errorMessage = "Failed to get value of CM IPv6 address from parameter";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify cm ip is obtained and logged with correct values");
			LOGGER.info(
					"STEP 5: ACTION : Execute command to get value of BroadBandTestConstants.TR181_PARAM_CM_IPV6\nExecute command:grep \"cm_prov=IPv6\" /rdklogs/logs/SelfHeal.txt.0\ngrep \"cm_ipv6=<cm_ip_param>\" /rdklogs/logs/SelfHeal.txt.0\ngrep \"cm_ip_status=1\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 5: EXPECTED : CM IP is obtained and logged with correct values");
			LOGGER.info("**********************************************************************************");

			paramIp = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandTestConstants.TR181_PARAM_CM_IPV6);
			if (CommonMethods.isNotNull(paramIp)) {
				errorMessage = "Failed to find cm_prov=IPv6 log message in SelfHeal.txt.0";
				if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_CM_PROV_IPV6, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
					errorMessage = "Failed to find cm IP logged in SelfHeal.txt.0";
					if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandCommonUtils.concatStringUsingStringBuffer(
									BroadBandTraceConstants.LOG_MESSAGE_CM_IPV6, paramIp),
							BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
						errorMessage = "Failed to find cm_ip_status=1 logged in SelfHeal.txt.0";
						status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
								BroadBandTraceConstants.LOG_MESSAGE_CM_IP_STATUS,
								BroadBandCommandConstants.LOG_FILE_SELFHEAL,
								BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : CM IP is obtained and logged with correct values");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s6";
			errorMessage = "Failed to get value of WAN IPv4 address from parameter";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify wan ip is obtained and logged with correct values");
			LOGGER.info(
					"STEP 6: ACTION : Execute command to get value of BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4 and BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6\nExecute command:grep \"wan_ipv4=<wan_ip4_param>\" /rdklogs/logs/SelfHeal.txt.0\ngrep \"wan_ipv6=<wan_ip6_param>\" /rdklogs/logs/SelfHeal.txt.0\ngrep \"wan_ip_status=1\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 6: EXPECTED : WAN IP is obtained and logged with correct values");
			LOGGER.info("**********************************************************************************");

			paramIp = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4);
			if (CommonMethods.isNotNull(paramIp)) {
				errorMessage = "Failed to find wan IPv4 address logged in SelfHeal.txt.0";
				if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTraceConstants.LOG_MESSAGE_WAN_IPV4,
								paramIp),
						BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
					errorMessage = "Failed to get value of WAN IPv6 address from parameter";
					paramIp = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);
					if (CommonMethods.isNotNull(paramIp)) {
						errorMessage = "Failed to find wan IPv6 address logged in SelfHeal.txt.0";
						if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
								BroadBandCommonUtils.concatStringUsingStringBuffer(
										BroadBandTraceConstants.LOG_MESSAGE_WAN_IPV6, paramIp),
								BroadBandCommandConstants.LOG_FILE_SELFHEAL,
								BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
							errorMessage = "Failed to find wan_ip_status=1 logged in SelfHeal.txt.0";
							status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
									BroadBandTraceConstants.LOG_MESSAGE_WAN_IP_STATUS,
									BroadBandCommandConstants.LOG_FILE_SELFHEAL,
									BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : WAN IP is obtained and logged with correct values");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s7";
			errorMessage = "Failed to ping4 status logged in SelfHeal.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify ping ipv4 status is logged");
			LOGGER.info("STEP 7: ACTION : Execute command:grep \"ping4_status=1\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 7: EXPECTED : Ping IPv4 status is obtained and logged");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PING_IPV4_STATUS, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Ping IPv4 status is obtained and logged");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s8";
			errorMessage = "Failed to find gw_health stored log message in SelfHeal.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify device compares current and previous wan status to determine reboot needed");
			LOGGER.info(
					"STEP 8: ACTION : Execute command:grep \"gw_health stored\" /rdklogs/logs/SelfHeal.txt.0grep \"gw_health current\" /rdklogs/logs/SelfHeal.txt.0grep \"IsNeedtoRebootDevice = 0\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 8: EXPECTED : Gw health stored and current is obtained and compared");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_GW_HEALTH_STORED, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
				errorMessage = "Failed to find gw_health current log message in SelfHeal.txt.0";
				if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_GW_HEALTH_CURRENT,
						BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
					errorMessage = "Failed to find isNeedtoRebootDevice log message in SelfHeal.txt.0";
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_NEED_TO_REBOOT,
							BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				}
			}

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Gw health stored and current is obtained and compared");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s9";
			errorMessage = "Failed to set value of Wan Link Heal enable to false";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Set value of Wan Link Heal enable to false");
			LOGGER.info(
					"STEP 9: ACTION : Execute webpa command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WANLinkHeal.Enable to false");
			LOGGER.info("STEP 9: EXPECTED : Value of Wan Link Heal enable parameter is false");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_LINK_HEAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Value of Wan Link Heal enable parameter is false");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s10";
			errorMessage = "Failed to revert device to original image: " + currentImage;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Perform CDL to original build on device");
			LOGGER.info(
					"STEP 10: ACTION : 1. Configure mock server with original image for estb mac of the device\n2. Set xconf check-in TR-181 parameter\n3. Wait for device to reboot and come up with original image");
			LOGGER.info("STEP 10: EXPECTED : Device is changed to original build successfully");
			LOGGER.info("**********************************************************************************");

			status = FirmwareDownloadUtils.performXconfHttpImageUpgrade(tapEnv, device, currentImage);

			if (status) {
				hasOriginalBuildChanged = status;
				LOGGER.info("STEP 10: ACTUAL : Device is changed to original build successfully");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s11";
			errorMessage = "Value of Wan Link Heal enable does not persist false after CDL";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify value of Wan Link Heal enable persists after CDL");
			LOGGER.info(
					"STEP 11: ACTION : Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WANLinkHeal.Enable");
			LOGGER.info("STEP 11: EXPECTED : Value of Wan Link Heal enable is false after CDL");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_LINK_HEAL_ENABLE, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Value of Wan Link Heal enable is false after CDL");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s12";
			errorMessage = "Wan Link Heal invoked log message is present when feature is disabled";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify Wan Link Heal is not invoked after CDL when it is disabled");
			LOGGER.info(
					"STEP 12: ACTION : Execute command:grep \"Wan Link Heal for bootup-check invoked\" /rdklogs/logs/SelfHeal.txt.0");
			LOGGER.info("STEP 12: EXPECTED : Wan link heal invoked log message is not present when disabled");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_WAN_LINK_HEAL_INVOKED,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Wan link heal invoked log message is not present when disabled");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION : Set value of Wan Link Heal enable to true");
			LOGGER.info(
					"POST-CONDITION 1: ACTION : Execute webpa command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WANLinkHeal.Enable to true");
			LOGGER.info("POST-CONDITION 1: EXPECTED : Value of Wan Link Heal enable parameter is true");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_LINK_HEAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("POST-CONDITION 1: ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION 1: ACTUAL : Post condition failed");
			}
			/**
			 * PostCondition 2: Revert back to original build
			 */
			if (!hasOriginalBuildChanged) {
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
						hasOriginalBuildChanged, BroadBandTestConstants.CONSTANT_1, currentImage);
			}

			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SELF-HEAL-4011");
	}

	/**
	 * Test to verify Critical process recovery for brlan0, brlan1, erouter0, peer
	 * ping, DHCPv4client, DHCPv6client, dnsmasq, Zombie process, dibbler
	 * 
	 * <li>1. Configure Aggressive selfheal & base selfheal interval as 2 & 3 min
	 * using Webpa</li>
	 * <li>2. Verify Aggressive selfheal interval is configured as 2 min using
	 * webpa</li>
	 * <li>3. Verify non-critical selfheal interval is 3 min using webpa</li>
	 * <li>4. Verify selfhealAggressive file present in device & updated interval in
	 * log file</li>
	 * <li>5. Make the brlan0 interface down and verify log in SelfHealAggressive
	 * file</li>
	 * <li>6. Verify brlan0 is not completely up status in SelfHeal file</li>
	 * <li>7. Make the brlan1 interface down and verify log in SelfHealAggressive
	 * file</li>
	 * <li>8. Verify brlan1 is not completely up status in SelfHeal file</li>
	 * <li>9. Verify dnsmasq process not running log message in SelfHealAggressive
	 * file</li>
	 * <li>10. Verify dnsmasq process not running log message in base SelfHeal
	 * file</li>
	 * <li>11. Verify dibbler is not running log message in SelfHealAggressive
	 * file</li>
	 * <li>12. Verify dibbler is not running status in SelfHeal file</li>
	 * <li>13. Trigger and verify dnsmasq zombie process in SelfHealAggressive
	 * file</li>
	 * <li>14. Verify dnsmasq zombie status in SelfHeal file</li>
	 * <li>15. Trigger and verify peer ping failure in SelfHealAggressive file</li>
	 * <li>16. Verify peer ping failure in SelfHeal file</li>
	 * <li>17. Make the erouter0 interface down and verify log in SelfHealAggressive
	 * file</li>
	 * <li>18. Verify erouter0 is DOWN status in SelfHeal file</li>
	 * <li>19. Trigger and verify DHCPv4client not running status in
	 * SelfHealAggressive file</li>
	 * <li>20. Verify DHCPv4client not running status in SelfHeal file</li>
	 * <li>21. Trigger and verify DHCPv6client not running status in
	 * SelfHealAggressive file</li>
	 * <li>22. Verify DHCPv6client not running status in SelfHeal file</li>
	 * <li>23. Update non-critical selfheal interval as 15 min using webpa</li>
	 * <li>24. Update selfheal aggressive interval as 5 min using webpa</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Said Hisham
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-AGGRESSIVE_SELF_HEAL-1002")
	public void testVerifyAggressiveSelfHeal(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-AGGRESSIVE_SELF_HEAL-102";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
		String peerInterface = null;
		String pid = null;
		String command = null;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-AGGRESSIVE_SELF_HEAL-1002");
		LOGGER.info(
				"TEST DESCRIPTION: Verify critical & non-critical processes are handling independently in SelfHealAggressive log file and SelfHeal file");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Configure Aggressive selfheal & base selfheal interval as 2 & 3 min using Webpa");
		LOGGER.info("2. Verify Aggressive selfheal interval is configured as 2 min using webpa");
		LOGGER.info("3. Verify non-critical selfheal interval is 3 min using webpa");
		LOGGER.info("4. Verify selfhealAggressive file present in device & updated interval in log file");
		LOGGER.info("5. Make the brlan0 interface down and verify log in SelfHealAggressive file");
		LOGGER.info("6. Verify brlan0 is not completely up status in SelfHeal file");
		LOGGER.info("7. Make the brlan1 interface down and verify log in SelfHealAggressive file");
		LOGGER.info("8. Verify brlan1 is not completely up status in SelfHeal file");
		LOGGER.info("9. Verify dnsmasq process not running log message in SelfHealAggressive file");
		LOGGER.info("10. Verify dnsmasq process not running log message in base SelfHeal file");
		LOGGER.info("11. Verify dibbler is not running log message in SelfHealAggressive file");
		LOGGER.info("12. Verify dibbler is not running status in SelfHeal file");
		LOGGER.info("13. Trigger and verify dnsmasq zombie process in SelfHealAggressive file");
		LOGGER.info("14. Verify dnsmasq zombie status in SelfHeal file");
		LOGGER.info("15. Trigger and verify peer ping failure in SelfHealAggressive file");
		LOGGER.info("16. Verify peer ping failure in SelfHeal file");
		LOGGER.info("17. Make the erouter0 interface down and verify log in SelfHealAggressive file");
		LOGGER.info("18. Verify erouter0 is DOWN status in SelfHeal file");
		LOGGER.info("19. Trigger and verify DHCPv4client not running status in SelfHealAggressive file");
		LOGGER.info("20. Verify DHCPv4client not running status in SelfHeal file");
		LOGGER.info("21. Trigger and verify DHCPv6client not running status in SelfHealAggressive file");
		LOGGER.info("22. Verify DHCPv6client not running status in SelfHeal file");
		LOGGER.info("23. Update non-critical selfheal interval as 15 min using webpa");
		LOGGER.info("24. Update selfheal aggressive interval as 5 min using webpa");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Failed to configure base & aggressive selfheal interval as 3 & 2 min using Webpa";
			status = false;
			boolean status1 = false;
			boolean status2 = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Configure Aggressive selfheal & base selfheal interval as 2 & 3 min using Webpa");
			LOGGER.info("STEP 1: ACTION :");
			LOGGER.info(
					"STEP 1: EXPECTED : Aggressive selfheal and base selfheal interval should be configured with 2 & 3 min successfully");
			LOGGER.info("**********************************************************************************");

			status1 = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			status2 = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_3, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			status = status1 && status2;
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully configured base & aggressive selfheal interval as 3 & 2 min using WEBPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################
			// //

			stepNum = "s2";
			errorMessage = "Failed to get the response from webpa or selfheal aggressive interval is not updated as 2 min";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify Aggressive selfheal interval is configured as 2 min using webpa");
			LOGGER.info(
					"STEP 2: ACTION : Execute webpa command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval");
			LOGGER.info("STEP 2: EXPECTED : Webpa get operation should be successful and response should be 2");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL,
					BroadBandTestConstants.STRING_CONSTANT_2, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully verified selfheal aggressive interval as 2 min using webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			// ##############################################################################################//

			stepNum = "s3";
			errorMessage = "Failed to get the response from webpa or selfheal base interval is not updated as 3 min";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify non-critical selfheal interval is 3 min using webpa");
			LOGGER.info(
					"STEP 3: ACTION : Execute webpa command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow");
			LOGGER.info("STEP 3: EXPECTED : Webpa get operation should be successful and response should be 3");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.STRING_CONSTANT_3,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully verified selfheal base interval as 3 min using webpa");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s4";
			errorMessage = "Failed to verify file availability or file not present in device or interval is not updated";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify selfhealAggressive file present in device & updated interval in log file");
			LOGGER.info("STEP 4: ACTION : Execute command: ls /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 4: EXPECTED : SelfHealAggressive.txt file should be present in the device");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isFileExists(device, tapEnv,
					BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT)
					&& CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_AGGR_SELFHEAL_INTERVAL,
							BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully verified SelfHealAggressive.txt file present in device");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			// ##############################################################################################//

			stepNum = "s5";
			errorMessage = "Failed to get the log message for brlan0 interface down in SelfHealAggressive file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Make the brlan0 interface down and verify log in SelfHealAggressive file");
			LOGGER.info(
					"STEP 5: ACTION : /sbin/ifconfig brlan0 down, grep -i \"brlan0 is not completely up\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 5: EXPECTED : Log message should be present in the SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			String brlan0Ip = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_BRLAN0_IP);
			if (CommonMethods.isNotNull(brlan0Ip)) {
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_BRLAN0_DOWN);
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_BRLAN0_DOWN,
						BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (CommonMethods.isNull(response)) {
					status = tapEnv.searchAndWaitForTraceFromStart(device, "brlan0 is not completely up",
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				}
			}
			if (CommonMethods.isNotNull(response) || status) {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_BRLAN0_RESTARTED,
						BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}
			if (status) {
				for (int iter = 0; iter < 3; iter++) {
					status = CommonUtils.isGivenStringAvailableInCommandOutput(
							tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_BRLAN0_IP),
							brlan0Ip);
					if (!status) {
						tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					} else {
						break;
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully verified brlan0 interface down log in SelfHealAggressive.txt file");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 6
			verifyLogInBaseSelfHealFile(device, testCaseId, 6, BroadBandTraceConstants.LOG_MESSAGE_BRLAN0_DOWN);

			if (!DeviceModeHandler.isBusinessClassDevice(device)) {
				stepNum = "s7";
				errorMessage = "Failed to get the log message for brlan1 interface down in SelfHealAggressive file";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 7: DESCRIPTION : Make the brlan1 interface down and verify log in SelfHealAggressive file");
				LOGGER.info(
						"STEP 7: ACTION : /sbin/ifconfig brlan1 down, grep -i \"brlan1 is not completely up\" /rdklogs/logs/SelfHealAggressive.txt");
				LOGGER.info("STEP 7: EXPECTED : Log message should be present in the SelfHealAggressive file");
				LOGGER.info("**********************************************************************************");

				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_BRLAN1_DOWN);
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_BRLAN1_DOWN,
						BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (CommonMethods.isNotNull(response)) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_BRLAN1_RESTARTED,
							BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				}

				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL : Successfully verified brlan1 interface down log in SelfHealAggressive.txt file");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				// ##############################################################################################//

				// step 8
				verifyLogInBaseSelfHealFile(device, testCaseId, 8, BroadBandTraceConstants.LOG_MESSAGE_BRLAN1_DOWN);
			} else {
				LOGGER.info("Step is not applicable for BusinessClass platform. Since brlan1 interface not available");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s7", ExecutionStatus.NOT_APPLICABLE,
						"Step is not applicable for BusinessClass platform. Since brlan1 interface not available",
						false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s8", ExecutionStatus.NOT_APPLICABLE,
						"Step is not applicable for BusinessClass platform. Since brlan1 interface not available",
						false);
			}

			stepNum = "s9";
			errorMessage = "Successfully got the log message for dnsmasq process not running in SelfHealAggressive file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify dnsmasq process not running log message in SelfHealAggressive file");
			LOGGER.info(
					"STEP 9: ACTION : kill -11 $(pidof dnsmasq), grep -i \"dnsmasq is not running\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 9: EXPECTED : Log message should not be present in the SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			pid = CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.DNSMASQ_PROCESS_NAME);
			if (CommonMethods.isNotNull(pid)) {
				CommonUtils.killTheProcessWithPid(device, tapEnv, pid);
				status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_DNSMASQ_NOT_RUN,
						BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Successfully verified dnsmasq not running log not present in SelfHealAggressive.txt file");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s10";
			errorMessage = "Failed to get the log message for dnsmasq process not running in base SelfHeal file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify dnsmasq process not running log message in base SelfHeal file");
			LOGGER.info("STEP 10: ACTION : grep -i \"dnsmasq is not running\" /rdklogs/logs/SelfHeal.txt");
			LOGGER.info("STEP 10: EXPECTED : Log message should be present in the SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_DNSMASQ_NOT_RUN, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Successfully verified dnsmasq not running log in base SelfHeal file");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s11";
			errorMessage = "Failed to get the log message for dibbler is process not running in SelfHealAggressive file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify dibbler is not running log message in SelfHealAggressive file");
			LOGGER.info(
					"STEP 11: ACTION : killall dibbler-server, grep -i \"dibbler is not running\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 11: EXPECTED : Log message should be present in the SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(
							BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS,
							BroadBandCommandConstants.PROCESS_DIBBLER_SERVER));
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_DIBBLER_NOT_RUN,
					BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Successfully verified dnsmasq not running log in SelfHealAggressive.txt file");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 12
			verifyLogInBaseSelfHealFile(device, testCaseId, 12, BroadBandTraceConstants.LOG_MESSAGE_DIBBLER_NOT_RUN);

			stepNum = "s13";
			errorMessage = "Failed to get the log message zombie instance of dnsmasq in selfheal aggressive log file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Trigger and verify dnsmasq zombie process in SelfHealAggressive file");
			LOGGER.info("STEP 13: ACTION : Execute command:"
					+ "1. $(dnsmasq -u nobody -q --clear-on-reload --bind-dynamic --add-mac --add-cpe-id=abcdefgh --dhcp-authoritative -P 4096 -C /var/dnsmasq.conf & exec /bin/sleep 5m) &"
					+ "2. grep -i \"Zombie instance of dnsmasq is present\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 13: EXPECTED : Should get the zombie instance for dnsmasq in SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_ZOMBIE_INSTANCE_FOR_DNSMASQ);
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_ZOMBIE_INSTANCE_OF_DNSMASQ,
					BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info(
						"STEP 13: ACTUAL : Successfully verified zombie instance of dnsmasq in selfheal aggressive log file");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 14
			verifyLogInBaseSelfHealFile(device, testCaseId, 14,
					BroadBandTraceConstants.LOG_MESSAGE_ZOMBIE_INSTANCE_OF_DNSMASQ);

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)
					|| BroadbandPropertyFileHandler.isDeviceCheckForSelfHeal(device)
					|| DeviceModeHandler.isBusinessClassDevice(device)) {
				stepNum = "s15";
				errorMessage = "Failed to down the peer interface or not getting ping to peer ip failed log in SelfHealAggressive log";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 15: DESCRIPTION : Trigger and verify peer ping failure in SelfHealAggressive file");
				LOGGER.info("STEP 15: ACTION : Execute command:"
						+ "1. grep -i \"PEER_INTERFACE_IP\" /etc/device.properties"
						+ "2. /sbin/ifconfig | grep -B 1 -i <interface ip>" + "3. /sbin/ifconfig <peer interface> down"
						+ "4. grep -i \"Ping to Peer IP failed\" /rdklogs/logs/SelfHealAggressive.txt");
				LOGGER.info(
						"STEP 15: EXPECTED : Successfully verified peer ping log message in SelfHealAggressive.txt file");
				LOGGER.info("**********************************************************************************");

				peerInterface = BroadBandCommonUtils.getPeerInterfaceNameForMultiCoreDevice(device, tapEnv);
				if (CommonMethods.isNotNull(peerInterface)) {
					command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_IFCONFIG,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, peerInterface,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.STRING_DOWN);
					tapEnv.executeCommandUsingSsh(device, command);
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_PING_TO_PEER_IP_FAILED,
							BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
					if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.CONSTANT_8)) {
						BroadBandCommonUtils.isSTBAccessible(tapEnv, device,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.CONSTANT_8);
					}
				}

				if (status) {
					LOGGER.info(
							"STEP 15: ACTUAL : Should get ping to peer ip failed log message in SelfHealAggressive log");
				} else {
					LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				// ##############################################################################################//

				// step 16
				verifyLogInBaseSelfHealFile(device, testCaseId, 16,
						BroadBandTraceConstants.LOG_MESSAGE_PING_TO_PEER_IP_FAILED);

			} else {
				LOGGER.info("Step is applicable for multicore platform devices");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s15", ExecutionStatus.NOT_APPLICABLE,
						"Step is applicable for multicore platform devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s16", ExecutionStatus.NOT_APPLICABLE,
						"Step is applicable for multicore platform devices", false);
			}

			stepNum = "s17";
			errorMessage = "Failed to get the log message for erouter0 interface down in SelfHealAggressive file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : Make the erouter0 interface down and verify log in SelfHealAggressive file");
			LOGGER.info(
					"STEP 17: ACTION : /sbin/ifconfig erouter0 down, grep -i \"erouter0 is DOWN\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info("STEP 17: EXPECTED : Log message should be present in the SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_EROUTER0_INTERFACE_DOWN);
			if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.CONSTANT_8)) {
				BroadBandCommonUtils.isSTBAccessible(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.CONSTANT_8);
			}
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_EROUTER0_DOWN,
					BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))
					|| tapEnv.searchAndWaitForTraceFromStart(device, "erouter0 is DOWN",
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			;

			if (status) {
				LOGGER.info(
						"STEP 17: ACTUAL : Successfully verified erouter0 is DOWN log in SelfHealAggressive.txt file");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 18
			verifyLogInBaseSelfHealFile(device, testCaseId, 18, BroadBandTraceConstants.LOG_MESSAGE_EROUTER0_DOWN);
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_EROUTER0_INTERFACE_UP);

			stepNum = "s19";
			errorMessage = "Failed to get the log message in selfheal aggressive log file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 19: DESCRIPTION : Trigger and verify DHCPv4client not running status in SelfHealAggressive file");
			LOGGER.info("STEP 19: ACTION : Execute command: 1. ps -ww | grep udhcpc | grep erouter"
					+ "2. kill -11 <pid> 3. grep -i \"DHCP Client for v4 is not running, need restart\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info(
					"STEP 19: EXPECTED : Should get the DHCP client for v4 not running log message in SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			if (BroadbandPropertyFileHandler.isDeviceCheckForSelfHeal(device)
					|| CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_TI_UDHCPC);
			} else {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_UDHCPC_EROUTER);
			}
			if (CommonMethods.isNotNull(response)) {
				pid = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_GET_PID_FROM_PS);
				if (CommonMethods.isNotNull(pid)) {
					CommonUtils.killTheProcessWithPid(device, tapEnv, pid);
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_DHCP_CLIENT_V4_NOT_RUNNING,
							BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
					if (!status) {
						status = CommonUtils.validateTraceLog(tapEnv, device,
								BroadBandTraceConstants.LOG_MESSAGE_DHCP_CLIENT_V4_NOT_RUNNING,
								BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, true);
					}
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 19: ACTUAL : Successfully verified DHCPv4client not running status in selfheal aggressive log file");
			} else {
				LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 20
			verifyLogInBaseSelfHealFile(device, testCaseId, 20,
					BroadBandTraceConstants.LOG_MESSAGE_DHCP_CLIENT_V4_NOT_RUNNING);

			stepNum = "s21";
			errorMessage = "Failed to get the log message in selfheal aggressive log file";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 21: DESCRIPTION : Trigger and verify DHCPv6client not running status in SelfHealAggressive file");
			LOGGER.info("STEP 21: ACTION : Execute command:" + "1. ps | grep dibbler-client" + "2. kill -11 <pid > "
					+ "3. grep -i \"DHCP Client for v6 is not running, need restart\" /rdklogs/logs/SelfHealAggressive.txt");
			LOGGER.info(
					"STEP 21: EXPECTED : Should get the DHCP client for v6 not running log message in SelfHealAggressive file");
			LOGGER.info("**********************************************************************************");

			if (BroadbandPropertyFileHandler.isDeviceCheckForSelfHeal(device)
					|| CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_TI_DHCP6C);
				pid = CommonMethods.isNotNull(response)
						? CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_GET_PID_FROM_PS)
						: null;
			} else {
				pid = BroadBandCommonUtils.getProcessIdForDibblerClient(device, tapEnv);
			}
			if (CommonMethods.isNotNull(pid)) {
				CommonUtils.killTheProcessWithPid(device, tapEnv, pid);
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_DHCP_V6_CLIENT_NOT_RUNNING,
						BroadBandCommandConstants.FILE_PATH_SELFHEAL_AGGRESSIVE_TXT,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}

			if (status) {
				LOGGER.info(
						"STEP 21: ACTUAL : Successfully verified DHCPv6client not running status in selfheal aggressive log file");
			} else {
				LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			// step 22
			verifyLogInBaseSelfHealFile(device, testCaseId, 22,
					BroadBandTraceConstants.LOG_MESSAGE_DHCP_V6_CLIENT_NOT_RUNNING);

			stepNum = "s23";
			errorMessage = "Failed to update base selfheal interval as 15 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 23: DESCRIPTION : Update non-critical selfheal interval as 15 min using webpa");
			LOGGER.info(
					"STEP 23: ACTION : Execute webpa set command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow uint 15");
			LOGGER.info("STEP 23: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_15, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 23: ACTUAL : Successfully updated base selfheal interval as 15 min using webpa");
			} else {
				LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

			stepNum = "s24";
			errorMessage = "Failed to update aggressive selfheal interval as 5 min using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 24: DESCRIPTION : Update selfheal aggressive interval as 5 min using webpa");
			LOGGER.info(
					"STEP 24: ACTION : Execute webpa set command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval uint 5");
			LOGGER.info("STEP 24: EXPECTED : Webpa set operation should be successful");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 24: ACTUAL : Successfully updated aggressive selfheal interval as 5 min using webpa");
			} else {
				LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			// ##############################################################################################//

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");

			LOGGER.info(
					"POST-CONDITION 1: DESCRIPTION : Revert Aggressive selfheal & base selfheal interval as Default values using Webpa ");
			LOGGER.info(
					"POST-CONDITION 1: ACTION : Configure Aggressive selfheal and base selfheal interval with 5 & 15 min respectively");
			LOGGER.info(
					"POST-CONDITION 1: EXPECTED : Aggressive selfheal and base selfheal interval should be configured with 5 & 15 min successfully");

			Boolean status1 = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			Boolean status2 = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_15, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

			status = status1 && status2;

			LOGGER.info("POST-CONDITION 1: post-condition status is " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-AGGRESSIVE_SELF_HEAL-1002");
	}

	public static void verifyLogInBaseSelfHealFile(Dut device, String testCaseId, int stepCount, String logMessage) {
		String stepNum = "s" + stepCount;
		String errorMessage = "Successfully logged " + logMessage + " in base selfheal file";
		boolean status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION : Verify " + logMessage + " status in SelfHeal file");
		LOGGER.info("STEP " + stepCount + ": ACTION : Execute command: grep -i " + logMessage
				+ " /rdklogs/logs/SelfHeal.txt.0");
		LOGGER.info("STEP " + stepCount + ": EXPECTED : Logs should not present in SelfHeal file");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device, logMessage,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

		if (status) {
			LOGGER.info("STEP " + stepCount + ": ACTUAL : Successfully verified " + logMessage
					+ " is NOT logged in base selfheal file");
		} else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		// ##############################################################################################//
	}

	/**
	 * Test to Validate if corrective action is taken (eRouter reboot only) when DNS
	 * resolve test failure case
	 *
	 * <ol>
	 * <p>
	 * <li>Pre-Condition 1: Tail the logs continuously from SelfHeal.txt.0 log file
	 * to get crash logs</li>
	 * <li>STEP 1: Reset erouter using webpa param
	 * \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" and verify reset
	 * status</li>
	 * <li>STEP 2: Enable DNS Ping test using webpa param
	 * \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" and verify enabled
	 * status</li>
	 * <li>STEP 3: Configure the invalid DNS PING test URL using webpa param
	 * \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_URL\" and verify reset status</li>
	 * <li>STEP 4:Enable the connectivity test corrective action by using command
	 * \"syscfg set ConnTest_CorrectiveAction true\", \"syscfg commit\"</li>
	 * <li>STEP 5: Verify telemetry logs for DNS ping test failures</li>
	 * <li>STEP 6: Check if 'erouter0' IP is released</li>
	 * </p>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Gnanaprakasham S
	 * @Refactor Rakesh C N
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.SYSTEM, BroadBandTestGroup.SELF_HEAL })
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4002")
	public void testVerifyDnsPingFailureForInvalidUrl(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-402";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;

		String response = null;

		LOGGER.info("STARTING TEST CASE: " + testId);

		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"TEST DESCRIPTION: Test to Validate if corrective action is taken (eRouter reboot only) when DNS resolve test failure case");
		LOGGER.info("Pre-Condition 1: Tail the logs continuously from SelfHeal.txt.0 log file to get crash logs");
		LOGGER.info(
				"STEP 1: Reset erouter using webpa param \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" and verify reset status");
		LOGGER.info(
				"STEP 2: Enable DNS Ping test using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" and verify enabled status  ");
		LOGGER.info(
				"STEP 3: Configure the invalid DNS PING test URL using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_URL\" and verify reset status  ");
		LOGGER.info(
				"STEP 4:Enable the connectivity test corrective action by using command \"syscfg set ConnTest_CorrectiveAction true\", \"syscfg commit\" ");
		LOGGER.info("STEP 5: Verify telemetry logs for DNS ping test failures ");
		LOGGER.info("STEP 6: Check if 'erouter0' IP is released ");
		LOGGER.info("#######################################################################################");

		/**
		 * As a pre condition tail the logs continuously from SelfHeal.txt.0 log file to
		 * get crash logs
		 */
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1: DESCRIPTION : Tail the logs continuously from SelfHeal.txt.0 log file to get crash logs");
			LOGGER.info("PRE-CONDITION 1: ACTION : Execute snmp mib for resourceUsageComputeWindow");
			LOGGER.info("PRE-CONDITION 1: EXPECTED : Expected result should contain value as 2");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to set pre condition for self heal test cases " + testId;
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandTestConstants.COMMAND_TO_GET_SELF_HEAL_LOGS_FOR_PROCESS_CRASH);

				status = BroadBandSelfHealUtils.executePreconditionForSelfHealTestScenario(device, tapEnv);
			} catch (Exception e) {
				errorMessage = errorMessage + " Exception occurred during execution : " + e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 1: ACTUAL : resourceUsageComputeWindow verified successfully for self heal test case");
			} else {
				LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");

			/**
			 * Reset erouter using webpa param
			 * "Device.X_CISCO_COM_DeviceControl.RebootDevice"
			 */
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Reset erouter using webpa param \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" and verify reset status");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa param \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" and verify reset status");
			LOGGER.info("STEP 1: EXPECTED : The response should return success message for reset");
			LOGGER.info("**********************************************************************************");
			try {
				errorMessage = "Not able to reset erouter using webpa param \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" ";
				status = BroadBandWiFiUtils.setWebPaParams(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_EROUTER_RESET, BroadBandTestConstants.STRING_ROUTER,
						WebPaDataTypes.STRING.getValue());
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Erouter reset status using webpa param Device.X_CISCO_COM_DeviceControl.RebootDevice : "
								+ status);
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			/**
			 * Enabled DNS test using webpa param
			 * Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable
			 */
			testStepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Enable DNS Ping test using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" and verify reset status");
			LOGGER.info(
					"STEP 2: ACTION : Execute webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" and verify reset status");
			LOGGER.info("STEP 2: EXPECTED : DNS PING test should be initiated successfully");
			LOGGER.info("**********************************************************************************");
			try {
				errorMessage = "Not able to enabled DNS ping test using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" ";
				status = BroadBandWiFiUtils.setWebPaParams(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DNS_PING_TEST_ENABLE, RDKBTestConstants.TRUE,
						WebPaDataTypes.BOOLEAN.getValue());

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Enable DNS Ping test status using webpa param Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable : "
								+ status);
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s3";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Configure the invalid DNS PING test URL using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_URL\" and verify reset status ");
			LOGGER.info(
					"STEP 3: ACTION : Update DNS URL as invalid url using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_URL\"");
			LOGGER.info("STEP 3: EXPECTED : Invalid DNS PING test URL must be set successfully");
			LOGGER.info("**********************************************************************************");
			try {
				errorMessage = "Not able to configure DNS ping url as invalid url using webpa param  \"Device.SelfHeal.X_RDKCENTRAL-COM_DNS_PINGTEST_Enable\" ";
				status = BroadBandWiFiUtils.setWebPaParams(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DNS_PING_TEST_URL,
						BroadBandTestConstants.STRING_DNS_PING_INVALID_URL, WebPaDataTypes.STRING.getValue());
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successsfully Configured the invalid DNS PING test URL using webpa param Device.SelfHeal.X_RDKCENTRAL-COM_DNS_URL : "
								+ status);
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s4";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Enable the connectivity test corrective action by using command \"syscfg set ConnTest_CorrectiveAction true\", \"syscfg commit\"");
			LOGGER.info(
					"STEP 4: ACTION : Enable the connectivity test corrective action by using command \"syscfg set ConnTest_CorrectiveAction true\", \"syscfg commit\" ");
			LOGGER.info("STEP 4: EXPECTED : Connectivity test corrective action is initiated successfully");
			LOGGER.info("**********************************************************************************");
			try {
				errorMessage = "Not able to enable the connectivity test corrective action by using command \"syscfg set ConnTest_CorrectiveAction true\", \"syscfg commit\"  ";
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandTestConstants.CMD_ENABLED_CONNECTIVITY_TEST_CORRECTIVE_ACTION);
				status = CommonMethods.isNull(response);

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Successfully enabled connectivity test corrective action using system commands : "
								+ status);
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s5";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify telemetry logs for DNS ping test failures");
			LOGGER.info("STEP 5: ACTION : Execute command cat /rdklogs/logs/SelfHeal.txt.0 | grep -i \"RDKB_REBOOT\"");
			LOGGER.info("STEP 5: EXPECTED : Telemetry logs must be present in SelfHeal.txt.0 file");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("Waiting for 10 minutes to get self heal logs");
			tapEnv.waitTill(AutomaticsConstants.TEN_MINUTE_IN_MILLIS);
			errorMessage = " Self Heal action is not logging any information about DNS ping test in SelfHeal.txt.0 log file";
			try {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_GET_DNS_PING_FAILURE_LOGS);
				status = CommonMethods.isNotNull(response)
						&& response.contains(BroadBandTestConstants.STRING_DNS_PING_FAILURE_SELF_HEAL_LOG);
			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 5: ACTUAL :The expected log has been found in the device. Actual: " + response);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			testStepNumber = "s6";
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Check if 'erouter0' IP is released ");
			LOGGER.info("STEP 6: ACTION : Execute command ifconfig | grep -i erouer0 and verify ip address");
			LOGGER.info("STEP 6: EXPECTED : Erouter module is reset and hence IP must be released as expected ");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Erouter IP is not release after erouter reset initiated by  webpa param \"Device.X_CISCO_COM_DeviceControl.RebootDevice \" ";
			response = BroadBandCommonUtils.getErouteripv6Address(device, tapEnv);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Erouter module is reset and hence IP released as expected");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 6 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			
		} catch (Exception exception) {
			errorMessage = "Exception occurred during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} finally {

			try {
				BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DNS_PING_TEST_URL,
						BroadBandTestConstants.EMPTY_STRING, WebPaDataTypes.STRING.getValue());
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			// Set resource usage computing value to default value 15.
//			BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
//					BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid(), SnmpDataType.INTEGER,
//					BroadBandTestConstants.STRING_VALUE_FIFTEEN);
			response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid(), SnmpDataType.INTEGER,
					BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty(), BroadBandTestConstants.STRING_VALUE_ZERO);
		}

	}

	/**
	 * Test to Verify that the MSO is able to configure the resource usage time
	 * window via SNMP
	 * <p>
	 * TEST STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1: Configure the resourceUsageComputeWindow to 3 using SNMP MIB
	 * (".1.3.6.1.4.1.17270.44.1.1.7.0")</li>
	 * <li>Step 2: Verify the resourceUsageComputeWindow value for self heal using
	 * SNMP mib ".1.3.6.1.4.1.17270.44.1.1.7.0"</li>
	 * <li>Step 3: Reboot the device and verify whether device comes up
	 * properly</li>
	 * <li>Step 4: Verify the resourceUsageComputeWindow value for self heal using
	 * SNMP mib ".1.3.6.1.4.1.17270.44.1.1.7.0"</li>
	 * <li>Step 5: Verify whether the resource computation happens in every 3
	 * minutes.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Parvathy
	 * @Refactor Alan_Bivera
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.SELF_HEAL })
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4009")
	public void testVerifyResourceUsageTimeWindowConfiguration(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-409";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// Hash map to store self heal configuration
		HashMap<String, String> selfHealConfiguration = new HashMap<>();
		String response = null;
		long stbTime = 0;
		// boolean isFactoryReset = false;
		long pollDuration = 0;
		long startTime = 0;
		boolean isFactoryReset = false;
		String SYSTEM_DATE_TIME_FORMAT_RETRIEVED = "HH:mm:ss";
		SimpleDateFormat formatter = new SimpleDateFormat(SYSTEM_DATE_TIME_FORMAT_RETRIEVED);
		SimpleDateFormat formatterForDate = new SimpleDateFormat(
				BroadBandTestConstants.SYSTEM_DATE_TIME_FORMAT_RETRIEVED);

		try {

			LOGGER.info("STARTING TEST CASE: " + testId);

			LOGGER.info(
					"###############################################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test to Verify that the MSO is able to configure the resource usage time window via SNMP");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"STEP 1: Configure the resourceUsageComputeWindow to 3 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.7.0\")");
			LOGGER.info(
					"STEP 2: Verify the resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info("STEP 3: Reboot the device and verify whether device comes up properly");
			LOGGER.info(
					"STEP 4: Verify the resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info("STEP 5: Verify whether the resource computation happens in every 3 minutes.");
			LOGGER.info(
					"#################################################################################################################");

			testStepNumber = "s1";
			errorMessage = "Not able to set resourceUsageComputeWindow to 3 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.7.0\")";
			status = false;
			LOGGER.info(
					"***********************************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Configure the resourceUsageComputeWindow to 3 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.7.0\")");
			LOGGER.info("STEP 1: ACTION : Execute a snmpset command on Mib resourceUsageComputeWindow to 3 mins value");
			LOGGER.info("STEP 1: EXPECTED : SNMP configurations must be successful");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
								BroadBandTestConstants.STRING_CONSTANT_3);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 1: ACTUAL : Successfully configured resource usage compute window value as 3 using WebPA");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				if (!CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
					BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL,
							BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_2,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
				}
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {
					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
								BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid(),
								SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_THREE);

						if (CommonMethods.isNotNull(response)) {
							errorMessage = "Failed to set resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0 ";
							status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
						} else {
							errorMessage = "Obtained null response..not able to set maxSubsystemResetCount parameter value using snmp mib  .1.3.6.1.4.1.17270.44.1.1.11 ";

						}

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

				if (status) {
					LOGGER.info(
							"STEP 1: ACTUAL : Successfully set the resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			}

			testStepNumber = "s2";
			errorMessage = "Failed to get the resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify the resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\"");
			LOGGER.info(
					"STEP 2: ACTION : Execute the snmpget command to get the value for resourceUsageComputeWindow value");
			LOGGER.info("STEP 2: EXPECTED : resourceUsageComputeWindow value for self heal should be set to 3.");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
						LOGGER.info("Resource Usage compute window retrieved using WebPa = " + response);
						status = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : Successfully obtained resource usage compute window value as 3 using WebPA");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {
					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);
						status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
								BroadBandTestConstants.STRING_RESOURCE_USAGE_FOR_SELF_HEAL,
								BroadBandTestConstants.STRING_VALUE_THREE);

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : Successfully obtained the resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			}

			testStepNumber = "s3";
			errorMessage = "Not able to access STB after reboot!!!!";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Reboot the device and verify whether device comes up properly");
			LOGGER.info("STEP 3: ACTION : Reboot the device");
			LOGGER.info("STEP 3: EXPECTED: Box should reboot and it should be accessable after reboot");
			LOGGER.info("**********************************************************************************");
			if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.STRING_CMD_DATE);
				errorMessage = "Unable to get the device date after executing date command in the device.";
				if (CommonMethods.isNotNull(response)) {
					try {
						Date date = formatterForDate.parse(response.trim());
						LOGGER.info("Date obtainted after success box reboot : " + date);
						stbTime = date.getTime();
						status = Long.valueOf(stbTime) != null;
					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				}
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully obtained IP address after reboot and box is accessible");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s4";
			errorMessage = "Failed to get the resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0 after device reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION :Verify the resourceUsageComputeWindow value for self heal using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.7.0\" after device reboot");
			LOGGER.info(
					"STEP 4: ACTION : Execute the snmpget command to get the value for resourceUsageComputeWindow value");
			LOGGER.info(
					"STEP 4: EXPECTED : resourceUsageComputeWindow value for self heal should be set to 3 after device reboot.");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
						LOGGER.info("Resource Usage compute window retrieved using WebPa = " + response);
						status = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 4: ACTUAL : Successfully obtained resource usage compute window value as 3 using WebPA");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {
					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);
						status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
								BroadBandTestConstants.STRING_RESOURCE_USAGE_FOR_SELF_HEAL,
								BroadBandTestConstants.STRING_VALUE_THREE);

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

				if (status) {
					LOGGER.info(
							"STEP 4: ACTUAL : Successfully obtained the resourceUsageComputeWindow value as 3 using snmp mib  .1.3.6.1.4.1.17270.44.1.1.7.0 after device reboot");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s5";
			errorMessage = "Failed to display the cpu usage after 3 mins";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify whether the resource computation happens in every 3 minutes. ");
			LOGGER.info("STEP 5: ACTION : CPU usage is displayed in every 3 minutes");
			LOGGER.info("STEP 5: EXPECTED :  CPU usage is displayed in every 3 minutes");
			LOGGER.info("**********************************************************************************");
			String upTime = null;
			int respIterationCount = 0, hitTimeIterationCount = 0;
			String resourceUsageResponse = null;
			upTime = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_UPTIME);
			startTime = System.currentTimeMillis();
			String[] responseArray = new String[BroadBandTestConstants.CONSTANT_2];
			LOGGER.info("Long.parseLong(upTime)  -" + Long.parseLong(upTime));
			pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS - Long.parseLong(upTime);
			// commands to be executed within the device
			try {
				response = tapEnv.executeCommandInSettopBox(device,
						BroadBandCommandConstants.COMMAND_TO_LIST_TAD_FROM_RUNNING_PROCESSES);
				LOGGER.info("COMMAND_TO_LIST_TAD_FROM_RUNNING_PROCESSES =" + response);
			} catch (Exception exception) {
				errorMessage = "Exception occured during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			// verifying whether the TAD process is up and running
			if (CommonMethods.isNotNull(response)) {
				status = CommonMethods.patternMatcher(response,
						BroadBandTestConstants.PATTERN_TO_GET_TAD_RUNNING_STATUS);
				LOGGER.info("PATTERN_TO_GET_TAD_RUNNING_STATUS =" + status);
				if (status) {

					tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
					// check ls /tmp/.resource_monitor_started
					try {
						response = null;
						LOGGER.info("System.currentTimeMillis()  =" + System.currentTimeMillis());
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.COMMAND_TO_FIND_RESOURCE_MONITOR_STARTED);
						LOGGER.info("COMMAND_TO_FIND_RESOURCE_MONITOR_STARTED =" + response);
					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
					if (CommonMethods.isNotNull(response)) {
						do {
							try {
								resourceUsageResponse = tapEnv.executeCommandInSettopBox(device,
										BroadBandTestConstants.COMMAND_TO_RETRIEVE_RESOURCE_USAGE);
								if (CommonMethods.isNotNull(resourceUsageResponse)) {
									LOGGER.info("ResourceUsageResponse ==== " + resourceUsageResponse);
									responseArray = resourceUsageResponse.split(BroadBandTestConstants.CHAR_NEW_LINE);
									LOGGER.info("responseArray length ==== " + responseArray.length);
									status = true;
								}
								// Breaks the do while loop when the size of response array exceeds 2
								if (responseArray.length >= BroadBandTestConstants.CONSTANT_2) {
									LOGGER.info("Size of ResourceUsageResponse Array " + responseArray.length);
									break;
								}
							} catch (Exception exception) {
								errorMessage = "Exception occured during execution :: " + exception.getMessage();
								LOGGER.error(errorMessage);
							}
						} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
								.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
					} else {
						errorMessage = "RESOURCE_MONITOR_STARTED file is not yet created";
					}
				} else {
					errorMessage = "TAD process is running,But Pattern mismatch";
				}
			} else {
				errorMessage = "There is no TAD running process";
			}
			if (status) {
				status = false;
				long timeDifference = BroadBandTestConstants.CONSTANT_0;
				Date hitTime[] = new Date[BroadBandTestConstants.CONSTANT_2];
				do {
					// get the time stamp from the response output
					responseArray[respIterationCount] = CommonMethods.patternFinder(responseArray[respIterationCount],
							BroadBandTestConstants.PATTERN_MATCH_FOR_TIME_FORMAT);
					LOGGER.info("The arr[i] =" + responseArray[respIterationCount]);
					if (CommonMethods.isNotNull(responseArray[respIterationCount])) {
						// convert the time stamp to formated output
						try {
							hitTime[hitTimeIterationCount] = formatter.parse(responseArray[respIterationCount]);
						} catch (ParseException e) {
							errorMessage = "Exception occured during time stamp formatting : " + e.getMessage();
							LOGGER.error(errorMessage);
						}
					}
					respIterationCount++;
					LOGGER.info("The " + respIterationCount + " log it time in selfHeal.txt ="
							+ hitTime[hitTimeIterationCount]);
					hitTimeIterationCount++;
					if (hitTimeIterationCount >= hitTime.length) {
						break;
					}
				} while (respIterationCount < responseArray.length);
				if (hitTime[0] != null && hitTime[1] != null) {

					timeDifference = hitTime[1].getTime() - hitTime[0].getTime();
					LOGGER.info("Time difference : " + TimeUnit.MILLISECONDS.toMinutes(timeDifference));
				}
				status = timeDifference >= BroadBandTestConstants.CONSTANT_3;
			}
			if (status) {
				LOGGER.info("STEP 5 : ACTUAL : CPU usage displayed after 3 minutes ");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = "Exception occured during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1 : DESCRIPTION : setting resourceUsageComputeWindow value to default value (15) ");
			LOGGER.info(
					"POST-CONDITION 1: ACTION : Execute a snmpset command on Mib resourceUsageComputeWindow to default 15 mins");
			LOGGER.info("POST-CONDITION 1: EXPECTED : SNMP Configuration must be successful");
			LOGGER.info("#######################################################################################");
			// as a post condition setting resourceUsageComputeWindow value to default value
			// (15)
			if (DeviceModeHandler.isDSLDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
						RESOURCE_USAGE_COMPUTE_WINDOW);
			} else {
//				response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
//						BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid(), SnmpDataType.INTEGER,
//						RESOURCE_USAGE_COMPUTE_WINDOW);
				response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv, BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid(), SnmpDataType.INTEGER, BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty(), BroadBandTestConstants.STRING_VALUE_ZERO);
				status = CommonMethods.isNotNull(response);
			}
			if (status) {
				LOGGER.info(
						"POST-CONDITION 1 : ACTUAL :Successfully set the resourceUsageComputeWindow value to default value (15)");
			} else {
				LOGGER.error(
						"POST-CONDITION 1 : ACTUAL : Failed to set the resourceUsageComputeWindow value to default value (15)");
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 2: DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
			LOGGER.info("POST-CONDITION 2: ACTION : BROAD BAND DEVICE REACTIVATION. ");
			LOGGER.info("POST-CONDITION 2: EXPECTED : device should get reactivated");
			LOGGER.info("#######################################################################################");
			if (isFactoryReset) {
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_2);
			}

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("COMPLETED TEST CASE: TC-RDKB-SELF-HEAL-4009");
	}

	/**
	 * Verification of setting invalid values to avgCPUThreshold and
	 * avgMemoryThreshold using SNMP MIB
	 * 
	 * <p>
	 * TEST STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1: Factory reset by using webpa to check default values</li>
	 * <li>Step 2: Verify avgCPUThreshold value for self heal after reboot using
	 * SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\"</li>
	 * <li>Step 3: Configure the avgCPUThreshold value to -150 using SNMP MIB
	 * (\".1.3.6.1.4.1.17270.44.1.1.8.0\"</li>
	 * <li>Step 4: Verify avgMemoryThreshold value for self heal using SNMP mib
	 * \".1.3.6.1.4.1.17270.44.1.1.9.0\"")</li>
	 * <li>Step 5:Configure the avgMemoryThreshold to -150 using SNMP MIB
	 * (\".1.3.6.1.4.1.17270.44.1.1.9.0\")")</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Parvathy
	 * @Refactor Alan_Bivera
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.SELF_HEAL })
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4010")
	public void testVerifyWatchdogInitiatedReboot(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-410";
		// Test step number
		String testStepNumber = "s1";
		// Hash map to store self heal configuration
		HashMap<String, String> selfHealConfiguration = new HashMap<>();
		// String to store the error message
		String errorMessage = null;
		String response = null;
		int stepNumber = 0;
		long pollDuration = 0;
		long startTime = 0;
		boolean isFactoryReset = false;
		LOGGER.info("STARTING TEST CASE: " + testId);

		LOGGER.info("########################################################################################");
		LOGGER.info(
				"TEST DESCRIPTION: Verification of setting invalid values to avgCPUThreshold and avgMemoryThreshold using SNMP MIB");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("STEP 1: Factory reset by using webpa to check default values");
		LOGGER.info(
				"STEP 2: Verify avgCPUThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
		LOGGER.info(
				"STEP 3: Configure the avgCPUThreshold value to -150 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.8.0\")");
		LOGGER.info(
				"STEP 4: Verify avgMemoryThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
		LOGGER.info(
				"STEP 5: Configure the avgMemoryThreshold value to -150 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.9.0\")");

		LOGGER.info("#########################################################################################");

		testStepNumber = "s1";
		errorMessage = "Unable to factory reset the device using the SNMP v3 Command";
		status = false;
		try {
			testStepNumber = "s1";
			errorMessage = "Unable to factory reset the device using the Webpa Command";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Factory reset using webpa  to check default values and Verify the device comes up after the Factory reset");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: Parameter: Device.X_CISCO_COM_DeviceControl.FactoryReset\n data type: string\n value: \"Router,Wifi,VoIP,Dect,MoCA\"");
			LOGGER.info(
					"STEP 1: EXPECTED : On executing this command, Factory Reset should be successful and Device should be up after successful Factory Reset");
			LOGGER.info("**********************************************************************************");
//			status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
//					BroadBandTestConstants.FOUR_MINUTES);
			status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
			if (status) {
				isFactoryReset = true;
				LOGGER.info("STEP 1: ACTUAL : successfully factory reset device");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			testStepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION :Verify avgCPUThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
			LOGGER.info(
					"STEP 2: ACTION : Execute the snmpget command to get the default avgCPUThreshold value for self heal");
			LOGGER.info("STEP 2: EXPECTED : Default avgCPUThreshold value for self heal should be 100");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_CPU_THRESHOLD);
						LOGGER.info("Average CPU threshold retrieved using WebPa = " + response);
						status = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(AVG_CPU_THRESHOLD_VALUE);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : Successfully obtained default avgCPUThreshold value for self heal using WebpA");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {

					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);

						status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
								BroadBandTestConstants.STRING_AVG_CPU_THRESHOLD_FOR_SELF_HEAL, AVG_CPU_THRESHOLD_VALUE);

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}

				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);
				errorMessage = "Failed to get default avgCPUThreshold value as 100. Expected value should be 100 but Actual obtained value is : "
						+ selfHealConfiguration.get(BroadBandTestConstants.STRING_AVG_CPU_THRESHOLD_FOR_SELF_HEAL);
				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : Successfully obtained default avgCPUThreshold value for self heal using SNMP mib  \".1.3.6.1.4.1.17270.44.1.1.8.0\"");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s3";
			status = false;
			LOGGER.info(
					"***********************************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Configure the avgCPUThreshold to -150 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.8.0\")");
			LOGGER.info("STEP 3: ACTION : Execute a snmpset command on Mib avgCPUThreshold");
			LOGGER.info("STEP 3: EXPECTED : Set should be not be successful as it is an invalid value");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						status = !BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.AVG_CPU_THRESHOLD, BroadBandTestConstants.CONSTANT_2,
								BroadBandTestConstants.STRING_VALUE_MINUS_ONE_FIFFY);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 3: ACTUAL : Invalid value is not set to avgCPUThreshold using WebPA");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {

					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

						response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
								BroadBandSnmpMib.ECM_SELFHEAL_AVG_CPU_THRESHOLD.getOid(), SnmpDataType.UNSIGNED_INTEGER,
								BroadBandTestConstants.STRING_VALUE_MINUS_ONE_FIFFY,
								BroadBandTestConstants.STRING_VALUE_ZERO);

						if (CommonMethods.isNotNull(response)) {
							errorMessage = "avgCPUThreshold is set with an invalid value";
							status = response.contains("Reason: wrongValue");
						} else {
							errorMessage = "Obtained null response";

						}

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

				if (status) {
					LOGGER.info(
							"STEP 3: ACTUAL : Invalid value is not set to avgCPUThreshold using snmp mib  .1.3.6.1.4.1.17270.44.1.1.8.0");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s4";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION :Verify avgMemoryThreshold value for self heal after reboot using SNMP mib \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
			LOGGER.info(
					"STEP 4: ACTION : Execute the snmpget command to get the default avgMemoryThreshold value for self heal");
			LOGGER.info("STEP 4: EXPECTED : Default avgMemoryThreshold value for self heal should be 100");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD);
						LOGGER.info("Average Memory threshold retrieved using WebPa = " + response);
						status = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(AVG_MEMORY_THRESHOLD_VALUE);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 4: ACTUAL : Successfully obtained default avgMemoryThreshold value for self heal using WebpA");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {

					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						selfHealConfiguration = BroadBandCommonUtils.retrieveSelfHealparameterValues(device, tapEnv);

						// verify avgMemoryThreshold details for self heal configuration
						// value should be 100
						status = BroadBandCommonUtils.validateSelfHealParameterValues(selfHealConfiguration,
								BroadBandTestConstants.STRING_AVG_MEMORY_THRESHOLD_FOR_SELF_HEAL,
								AVG_MEMORY_THRESHOLD_VALUE);

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}

				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);
				errorMessage = "Failed to get default avgMemoryThreshold value as 100. Expected value should be 100 but Actual obtained value is : "
						+ selfHealConfiguration.get(BroadBandTestConstants.STRING_AVG_MEMORY_THRESHOLD_FOR_SELF_HEAL);
				if (status) {
					LOGGER.info(
							"STEP 4: ACTUAL : Successfully obtained default avgMemoryThreshold value for self heal using SNMP mib  \".1.3.6.1.4.1.17270.44.1.1.9.0\"");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			testStepNumber = "s5";
			status = false;
			LOGGER.info(
					"***********************************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Configure the avgMemoryThreshold to -150 using SNMP MIB (\".1.3.6.1.4.1.17270.44.1.1.8.0\")");
			LOGGER.info("STEP 5: ACTION : Execute a snmpset command on Mib avgMemoryThreshold");
			LOGGER.info("STEP 5: EXPECTED : Set should be not be successful as it is an invalid value");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				try {
					pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
					startTime = System.currentTimeMillis();
					do {
						status = !BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD, BroadBandTestConstants.CONSTANT_2,
								BroadBandTestConstants.STRING_VALUE_MINUS_ONE_FIFFY);
					} while ((System.currentTimeMillis() - startTime) < pollDuration && BroadBandCommonUtils
							.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS) && !status);
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 5: ACTUAL : Invalid value is not set to avgMemoryThreshold using WebPA");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {

					try {
						LOGGER.info("GOING TO WAIT FOR 2 MINUTE.");
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

						response = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
								BroadBandSnmpMib.ECM_SELFHEAL_AVG_MEMORY_THRESHOLD.getOid(),
								SnmpDataType.UNSIGNED_INTEGER, BroadBandTestConstants.STRING_VALUE_MINUS_ONE_FIFFY,
								BroadBandTestConstants.STRING_VALUE_ZERO);

						if (CommonMethods.isNotNull(response)) {
							errorMessage = "avgMemoryThreshold is set with an invalid value";
							status = response.contains("Reason: wrongValue");
						} else {
							errorMessage = "Obtained null response";

						}

					} catch (Exception exception) {
						errorMessage = "Exception occured during execution : " + exception.getMessage();
						LOGGER.error(errorMessage);
					}
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !status);

				if (status) {
					LOGGER.info(
							"STEP 5: ACTUAL : Invalid value is not set to avgMemoryThreshold using snmp mib  .1.3.6.1.4.1.17270.44.1.1.8.0");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}
		} catch (Exception exception) {
			errorMessage = "Exception occured during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
			LOGGER.info("POST-CONDITION 1: ACTION : BROAD BAND DEVICE REACTIVATION. ");
			LOGGER.info("POST-CONDITION 1: EXPECTED : device should get reactivated");
			LOGGER.info("#######################################################################################");
			if (isFactoryReset) {
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_1);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}

		LOGGER.info("COMPLETED TEST CASE: TC-RDKB-SELF-HEAL-4010");
	}

	/**
	 * Test to Validate if the cron scheduling on ATOM side whether it is having
	 * LogUploadFrequency value using snmp mibs
	 * 
	 * <ol>
	 * <p>
	 * <li>STEP 1 :Enable diagnostic mode using snmp mib</li>
	 * <li>STEP 2 :verify logupload frequency using snmp mib</li>
	 * <li>STEP 3 :verify logupload frequency from atom console</li>
	 * <li>STEP 4 :Configure log upload frequency using snmp mib</li>
	 * <li>STEP 5 :verify logupload frequency using snmp mib</li>
	 * <li>STEP 6 :verify logupload frequency from atom console</li>
	 * <li>STEP 7 :Disable diagnostic mode using snmp mib</li>
	 * <li>STEP 8 :verify default value logupload frequency using snmp mib</li>
	 * </p>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Gnanaprakasham S
	 * @refactor Rakesh C N, Sruthi Santhosh
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SELF-HEAL-4005")
	public void testVerifyLogUploadFrequencyForCronSchedulingOnAtomSideUsingSnmp(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-SELF-HEAL-405";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;

		try {

			LOGGER.info("STARTING TEST CASE: " + testId);

			LOGGER.info("**************************************************************");
			LOGGER.info("TEST DESCRIPTION: Test to verify self heal diagnostic mode test scenarios using webpa params");
			LOGGER.info("*************************************************************************");

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1: Enable the diagnostic mode using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" and verify enabled status ");
			LOGGER.info("EXPECTED: Diagnostic mode must be enabled via snmp mib ");
			LOGGER.info("STEP 2: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("EXPECTED: Log upload frequency value must be in default value 1440 ");
			LOGGER.info("STEP 3: Check the cron scheduling on ATOM side whether it is having LogUploadFrequency value");
			LOGGER.info(
					"EXPECTED: Response should contains Logupload frequency value and it should be same as step 2 response ");
			LOGGER.info(
					"STEP 4:Configure log upload frequency as 60 using snmp oid \".1.3.6.1.4.1.17270.44.1.1.13.0\"");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info("STEP 5: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"STEP 6: Check the cron scheduling on ATOM side whether it is having updated LogUploadFrequency value");
			LOGGER.info("EXPECTED: Response should contains Logupload frequency as 60 ");
			LOGGER.info(
					"STEP 7: Disable diagnostic mode using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" and verify disabled status ");
			LOGGER.info("EXPECTED: Diagnostic mode must be disabled via snmp mib ");
			LOGGER.info("STEP 8: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");

			LOGGER.info(
					"************************************************************************************************");

			if (!CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				LOGGER.info("It is applicable only for Atom devices. Hence skipping the execution!!!");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, true);
			}

			// Enabled diagnostic mode using snmp mib
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1: Enable the diagnostic mode using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" and verify enabled status ");
			LOGGER.info(
					"ACTION: Execute snmp mib .1.3.6.1.4.1.17270.44.1.1.12.0 and verify diagnostic mode enabled status");
			LOGGER.info("EXPECTED: Diagnostic mode must be enabled via snmp mib ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to enable diagnostic mode using using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" ";
				status = BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_ONE);
				LOGGER.info("Diagnostic mode enabled status via snmp mib : " + status);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S1 ACTUAL RESULT : " + (status
					? "SUCCESSFULLY ENABLED DIAGNOSTICS MODE USING SNMP MIB .1.3.6.1.4.1.17270.44.1.1.12.0 !!!"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s2";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 2: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("ACTION: Execute snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 and verify log upload frequency");
			LOGGER.info("EXPECTED: Log upload frequency value must be in diagnostic mode default value 1440 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Log upload frequency is not in diagnostic mode default value (1440) after enabling diagnostic mode.. ";
				status = BroadBandSelfHealUtils.verifyLogUploadFrequencyUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_1440);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S2 ACTUAL RESULT : " + (status
					? "SUCCESSFULLY VERIFIED LOG UPLOAD FREQUENCY IS NOT IN DIAGNOSTIC MODE DEFAULT VALUE (1440)"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s3";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 3: Check the cron scheduling on ATOM side whether it is having LogUploadFrequency value");
			LOGGER.info("ACTION: Execute command cat /tmp/cron/root and verify log upload frequency");
			LOGGER.info(
					"EXPECTED: Response should contains Logupload frequency value and it should be same as step 2 response ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Failed to check the cron scheduling on ATOM side having LogUploadFrequency value";
				status = BroadBandSelfHealUtils.verifyLogUploadFrequencyOnAtomConsole(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_1440);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S3 ACTUAL RESULT : "
					+ (status ? "SUCCESSFULLY VERIFIED DEFAULT VALUE FOR LOG UPLOAD FREQUENCY FROM ATOM CONSOLE !!!"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s4";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 4:Configure log upload frequency as 60 using snmp oid \".1.3.6.1.4.1.17270.44.1.1.13.0\"");
			LOGGER.info(
					"ACTION: Set log upload frequency value as 60 by executing snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Not able to configure log upload frequency using snmp mib \".1.3.6.1.4.1.17270.44.1.1.13.0\" ";
				status = BroadBandSelfHealUtils.configureLogUploadFrequencyUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_60);

			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S4 ACTUAL RESULT : " + (status
					? "SUCCESSFULLY CONFIGURED LOG UPLOAD FREQUENCY USING SNMP MIB .1.3.6.1.4.1.17270.44.1.1.13.0 !!!"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s5";
			status = false;

			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 5: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info(
					"ACTION: Executing snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 and verify log upload frequency value");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				errorMessage = "Failed to get the updtated log uplod frequency using snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 ";
				status = BroadBandSelfHealUtils.verifyLogUploadFrequencyUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_60);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S5 ACTUAL RESULT : "
					+ (status ? "SUCCESSFULLY VERIFIED CONFIGURED LOG UPLOAD FREQUENCY USING SNMP MIB !!! "
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s6";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 6: Check the cron scheduling on ATOM side whether it is having LogUploadFrequency value");
			LOGGER.info("ACTION: Execute command cat /tmp/cron/root and verify log upload frequency");
			LOGGER.info(
					"EXPECTED: Response should contains Logupload frequency value and it should be same as step 2 response ");
			LOGGER.info(
					"************************************************************************************************");

			try {
				errorMessage = "Failed to check the cron scheduling on ATOM side having LogUploadFrequency value";
				status = BroadBandSelfHealUtils.verifyLogUploadFrequencyOnAtomConsole(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_60);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			LOGGER.info("S6 ACTUAL RESULT : "
					+ (status ? "SUCCESSFULLY VERIFIED UPDATED VALUE FOR LOG UPLOAD FREQUENCY FROM ATOM CONSOLE !!!"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s7";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 7: Disable diagnostic mode using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" and verify disabled status ");
			LOGGER.info(
					"ACTION: Execute snmp mib .1.3.6.1.4.1.17270.44.1.1.12.0 and verify diagnostic mode enabled status");
			LOGGER.info("EXPECTED: Diagnostic mode must be disabled via snmp mib ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				errorMessage = "Not able to disable diagnostic mode using  using snmp mib \".1.3.6.1.4.1.17270.44.1.1.12.0 \" ";
				status = BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_TWO);
				LOGGER.info("Diagnostic mode disabled status via snmp mib : " + status);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			LOGGER.info("S7 ACTUAL RESULT : "
					+ (status ? "SUCCESSFULLY DISABLED DIAGNOSTIC MODE USING SNMP MIB !!!" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s8";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 8: Verify log upload frequency value using snmp oid .1.3.6.1.4.1.17270.44.1.1.13.0 ");
			LOGGER.info("ACTION: Execute snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 and verify log upload frequency");
			LOGGER.info("EXPECTED: Log upload frequency value must be 60 ");
			LOGGER.info(
					"************************************************************************************************");

			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				errorMessage = "Failed to get the configured value for log uplod frequency using snmp mib .1.3.6.1.4.1.17270.44.1.1.13.0 ";
				status = BroadBandSelfHealUtils.verifyLogUploadFrequencyUsingSnmpMib(device, tapEnv,
						BroadBandTestConstants.STRING_VALUE_60);
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S8 ACTUAL RESULT : "
					+ (status ? "SUCCESSFULLY CONFIGURED VALUE FOR LOG UPLOAD FREQUENCY USING SNMP MIB !!!"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = "Exception occurred during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} finally {
			BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
					AutomaticsConstants.FALSE);
			BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY,
					WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.STRING_VALUE_1440);
		}
		LOGGER.info("ENDING TEST CASE: " + testId);
	}

	/**
	 * Test to verify single client harvester report configurations
	 * 
	 * <li>1. Verify wifi client schema name is readonly access</li>
	 * <li>2. Verify wifi client schema id is readonly access</li>
	 * <li>3. Verify wifiClient mac address length using webpa</li>
	 * <li>4. Update wifiClient mac address using webpa</li>
	 * <li>5. Update override TTL value as 901 using webpa</li>
	 * <li>6. Update reporting period value as 70 using webpa. Applicable values are
	 * 1,5,15,30,60,300,900,1800,3600,10800,21600,43200,86400 (in seconds)</li>
	 * <li>7. Update Reporting Period as 5 and override TTL value as 15 using
	 * webpa</li>
	 * <li>8. Update wifiClient enable status as true using webpa</li>
	 * <li>9. Verify override TTL value reset to 0 after expiry of 15 seconds using
	 * webpa for polled duration of 30 seconds</li>
	 * <li>10. Update override TTL value to maximum limit as 900 using webpa</li>
	 * <li>11. Update reporting period value as 3600 using webpa</li>
	 * <li>12. Reboot the device to verify persistance of value</li>
	 * <li>13. Verify reporting period value is persist after reboot</li>
	 * <li>14. Verify override TTL value reset to 0 after reboot</li>
	 * <li>15. Verify wifiClient enable status persist after reboot</li>
	 * <li>16. Verify wifiClient mac address value persist after reboot</li>
	 * <li>17. Update reporting period value as 0 using webpa</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * 
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-HARVESTER-REPORT-1001")
	public void testToVerifySingleClientHarvesterConfig(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-HARVESTER-REPORT-1001");
		LOGGER.info("TEST DESCRIPTION: Test to verify single client harvester report configuration");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify wifi client schema name is readonly access");
		LOGGER.info("2. Verify wifi client schema id is readonly access");
		LOGGER.info("3. Verify wifiClient mac address length using webpa");
		LOGGER.info("4. Update wifiClient mac address using webpa");
		LOGGER.info("5. Update override TTL value as 901 using webpa");
		LOGGER.info(
				"6. Update reporting period value as 70 using webpa. Applicable values are 1,5,15,30,60,300,900,1800,3600,10800,21600,43200,86400 (in seconds)");
		LOGGER.info("7. Update Reporting Period as 5 and override TTL value as 15 using webpa");
		LOGGER.info("8. Update wifiClient enable status as true using webpa");
		LOGGER.info(
				"9. Verify override TTL value reset to 0 after expiry of 15 seconds using webpa for polled duration of 30 seconds");
		LOGGER.info("10. Update override TTL value to maximum limit as 900 using webpa");
		LOGGER.info("11. Update reporting period value as 3600 using webpa");
		LOGGER.info("12. Reboot the device to verify persistance of value");
		LOGGER.info("13. Verify reporting period value is persist after reboot");
		LOGGER.info("14. Verify override TTL value reset to 0 after reboot");
		LOGGER.info("15. Verify wifiClient enable status persist after reboot");
		LOGGER.info("16. Verify wifiClient mac address value persist after reboot");
		LOGGER.info("17. Update reporting period value as 0 using webpa");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-HARVESTER-REPORT-101";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		int reportingPeriod = BroadBandTestConstants.CONSTANT_0;
		int overrideTTL = BroadBandTestConstants.CONSTANT_0;
		int waitTime = BroadBandTestConstants.CONSTANT_0;
		// variable declaration ends

		try {

			stepNumber = "s1";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Verify wifi client schema name is readonly access");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Schema data type: 0 value: SingleClient.avsc");
			LOGGER.info("STEP 1: EXPECTED: Webpa set operation should be failed");
			LOGGER.info("******************************************************************************");
			errorMessage = "WifiClient schema name having write access";
			status = !BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_SCHEMA, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.INCORRECT_SCHEMA_NAME);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully verified wifi client schema is readonly access");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify wifi client schema id is readonly access");
			LOGGER.info(
					"STEP 2: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.SchemaID data type: 0 value: TestString");
			LOGGER.info("STEP 2: EXPECTED: Webpa set operation should be failed");
			LOGGER.info("******************************************************************************");
			errorMessage = "WifiClient schema id having write access";
			status = !BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_SCHEMA_ID, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.TEST_SSID_PASSWORD);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified wifi client schema id is readonly access");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Verify wifiClient mac address length using webpa");
			LOGGER.info(
					"STEP 3: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress");
			LOGGER.info(
					"STEP 3: EXPECTED: Should get the response for webpa parameter and response should be of length 12");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress";
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS);
			status = CommonMethods.isNotNull(response)
					&& response.trim().length() == BroadBandTestConstants.CONSTANT_12;
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully verified wifi client mac address length using webpa");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Update wifiClient mac address using webpa");
			LOGGER.info(
					"STEP 4: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress data type: 0 value: "+tapEnv.getStbMacIdForIpDeviceWithoutColon(device));
			LOGGER.info("STEP 4: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress";

			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS, BroadBandTestConstants.CONSTANT_0,
					tapEnv.getStbMacIdForIpDeviceWithoutColon(device));

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL: Successfully updated the wifi client mac address using webpa set operation");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Update override TTL value as 901 using webpa");
			LOGGER.info(
					"STEP 5: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL data type: 2 value: 901");
			LOGGER.info("STEP 5: EXPECTED: Webpa set operation should be failed");
			LOGGER.info("******************************************************************************");
			errorMessage = "Successfully updated the override TTL value above maximum value";
			status = !BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_901);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Successfully verified maximum value of override TTL value is 900");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION: Update reporting period value as 70 using webpa. Applicable values are 1,5,15,30,60,300,900,1800,3600,10800,21600,43200,86400 (in seconds)");
			LOGGER.info(
					"STEP 6: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod data type: 2 value: 70");
			LOGGER.info("STEP 6: EXPECTED: Webpa set operation should be failed");
			LOGGER.info("******************************************************************************");
			errorMessage = "Successfully updated the reporting period value using webpa";
			status = !BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_VALUE_70);
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL: Successfully verified wifi client reporting period is not set to invalid value");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Update Reporting Period as 5 and override TTL value as 15 using webpa");
			LOGGER.info(
					"STEP 7: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod datatype: 2 value : 5 and  parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL data type: 2 value: 15");
			LOGGER.info("STEP 7: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");

			try {
				errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod";
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_5);
				if (status) {
					errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL";
					status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
							BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_15);
				}
				reportingPeriod = Integer.parseInt(tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD));
				overrideTTL = Integer.parseInt(tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL));
				// calculation
				// (ReportingPeriod * ((OverrideTTL / ReportingPeriod) + 1)) time.

				waitTime = (reportingPeriod * ((overrideTTL / reportingPeriod) + 1));
				LOGGER.info("WaitTime " + waitTime);
			} catch (Exception e) {
				LOGGER.error("Exception occured during execution " + e.getMessage());
			}
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL: Successfully updated the wifi client Reporting Period and default override ttl value");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// ##################################################################################################//

			stepNumber = "s8";
			status = false;

			// ##################################################################################################//

			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Update wifiClient enable status as true using webpa");
			LOGGER.info(
					"STEP 8: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled datatype: boolean value: true");
			LOGGER.info("STEP 8: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully updated the wifi client enabled status as true");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNumber = "s9";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION: Verify override TTL value reset to 0 after expiry of 15 seconds using webpa for polled duration of 30 seconds");
			LOGGER.info(
					"STEP 9: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL");
			LOGGER.info("STEP 9: EXPECTED: Webpa response become 0 after expiry of override TTL value");
			LOGGER.info("******************************************************************************");
			errorMessage = "Override TTL value not resetting to 0";

			long waitTimeInMilliSeconds = waitTime * 1000 + BroadBandTestConstants.TEN_SECOND_IN_MILLIS;
			LOGGER.info("Going to wait time for " + waitTimeInMilliSeconds + " Milliseconds");
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
					BroadBandTestConstants.STRING_ZERO, waitTimeInMilliSeconds,
					BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL: Successfully verified wifi client override TTL value reset to 0 after expiry of TTL value from 15");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s10";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION: Update override TTL value to maximum limit as 900 using webpa");
			LOGGER.info(
					"STEP 10: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL data type: 2 value: 900");
			LOGGER.info("STEP 10: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
					BroadBandTestConstants.CONSTANT_2, Integer.toString(BroadBandTestConstants.CONSTANT_900));
			if (status) {
				LOGGER.info("STEP 10: ACTUAL: Successfully updated override TTL value to maximum limit as 900");
			} else {
				LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s11";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION: Update reporting period value as 3600 using webpa");
			LOGGER.info(
					"STEP 11: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod data type: 2 value: 3600");
			LOGGER.info("STEP 11: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_3600);
			if (status) {
				LOGGER.info("STEP 11: ACTUAL: Successfully updated wifi client reporting period value as 3600");
			} else {
				LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s12";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION: Reboot the device to verify persistance of value");
			LOGGER.info("STEP 12: ACTION: Execute command: /bin/reboot");
			LOGGER.info("STEP 12: EXPECTED: Device should be SSHable after reboot");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to reboot the device or unable to SSH after reboot";
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("STEP 12: ACTUAL: Successfully rebooted the device and able to SSH after reboot");
			} else {
				LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s13";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION: Verify reporting period value is persist after reboot");
			LOGGER.info(
					"STEP 13: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod");
			LOGGER.info("STEP 13: EXPECTED: Should get the response as 3600 after reboot");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter or value is not persistent after reboot";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD,
					BroadBandTestConstants.STRING_CONSTANT_3600, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL: Successfully verified reporting period value is persistent after reboot");
			} else {
				LOGGER.error("STEP 13: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s14";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION: Verify override TTL value reset to 0 after reboot");
			LOGGER.info(
					"STEP 14: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL");
			LOGGER.info("STEP 14: EXPECTED: should get the response as 0 after reboot");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter or value is persistent after reboot";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
					BroadBandTestConstants.STRING_ZERO, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 14: ACTUAL: Successfully verified override TTL value reset to 0 after reboot");
			} else {
				LOGGER.error("STEP 14: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s15";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION: Verify wifiClient enable status persist after reboot");
			LOGGER.info(
					"STEP 15: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled");
			LOGGER.info("STEP 15: EXPECTED: Should get the response as true after reboot");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter or value is not persistent after reboot";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 15: ACTUAL: Successfully verified wifi client enabled status is persistent after reboot");
			} else {
				LOGGER.error("STEP 15: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s16";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION: Verify wifiClient mac address value persist after reboot");
			LOGGER.info(
					"STEP 16: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress");
			LOGGER.info("STEP 16: EXPECTED: Should get the response as client mac address");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter or value is not persistent after reboot";

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS,
					tapEnv.getStbMacIdForIpDeviceWithoutColon(device), BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info(
						"STEP 16: ACTUAL: Successfully verified wifi client mac address is persistent after reboot");
			} else {
				LOGGER.error("STEP 16: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s17";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 17: DESCRIPTION: Update reporting period value as 0 using webpa");
			LOGGER.info(
					"STEP 17: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod data type: 2 value: 0");
			LOGGER.info("STEP 17: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_ZERO, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 17: ACTUAL: Successfully updated the reporting period value as 0");
			} else {
				LOGGER.error("STEP 17: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"Exception Occurred while Verifying single client harvester report configuration" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-HARVESTER-REPORT-1001");
		// ###############################################################//
	}

}
